#include "VStandaloneSimulator.h"
#include "verilated.h"

#include <cstdint>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

static uint64_t number(const std::string &s) {
    size_t end;
    if (s.empty() || s[0] == '-') throw std::runtime_error("invalid number: " + s);
    auto value = std::stoull(s, &end, 0);
    if (end != s.size()) throw std::runtime_error("invalid number: " + s);
    return value;
}

int main(int argc, char **argv) {
    try {
        std::string program, plan, format = "bytes";
        uint64_t maxCycles = 0, index = 32, expected = 0, thread = 0;
        for (int i = 1; i < argc; ++i) {
            std::string option = argv[i];
            auto next = [&]() -> std::string {
                if (++i == argc) throw std::runtime_error("missing option argument");
                return argv[i];
            };
            if (option == "--program") program = next();
            else if (option == "--plan") plan = next();
            else if (option == "--format") format = next();
            else if (option == "--max-cycles") maxCycles = number(next());
            else if (option == "--thread") thread = number(next());
            else if (option == "--expect-register") {
                index = number(next());
                expected = number(next());
            } else throw std::runtime_error("unknown option: " + option);
        }
        if (program.empty() || (plan.empty() && (!maxCycles || index > 31)))
            throw std::runtime_error("usage: b4smt-sim --program FILE.hex [--plan FILE | --max-cycles N --expect-register INDEX VALUE] [--format bytes|words|binary]");
        if (format != "bytes" && format != "words" && format != "binary")
            throw std::runtime_error("invalid program format");

        std::ifstream input(program, std::ios::binary);
        if (!input) throw std::runtime_error("cannot open program: " + program);
        std::vector<uint64_t> words;
        size_t bytes = 0;
        auto byte = [&](uint64_t value) {
            if (bytes >= 512 * 1024 * 1024) throw std::runtime_error("program exceeds fixture memory");
            if (bytes % 8 == 0) words.push_back(0);
            words.back() |= value << (8 * (bytes % 8));
            ++bytes;
        };
        std::string token;
        if (format == "binary") {
            char c;
            while (input.get(c)) byte(static_cast<unsigned char>(c));
        } else while (input >> token) {
            if (token[0] == 'x') token.erase(0, 1);
            if ((format == "bytes" ? token.size() != 2 : token.size() > 16 || token.empty()) ||
                token.find_first_not_of("0123456789abcdefABCDEF") != std::string::npos)
                throw std::runtime_error("invalid hex token: " + token);
            auto value = std::stoull(token, nullptr, 16);
            if (format == "bytes") byte(value);
            else {
                for (int i = 0; i < 8; ++i) byte((value >> (8 * i)) & 255);
            }
        }
        if (!input.eof() || words.empty()) throw std::runtime_error("empty or unreadable program");

        VerilatedContext context;
        context.commandArgs(argc, argv);
        context.assertOn(true);
        VStandaloneSimulator top{&context};
        top.clock = 0;
        top.reset = 1;
        top.loadValid = 0;
        top.loadData = 0;
        top.registerIndex = 0;
        top.threadIndex = thread;
        // The sample CLI streams continuously. Test plans instead start with
        // backpressure, exactly as the fixture's loadMemory/getOutput protocol.
        top.uartReady = plan.empty();
        top.eval();
        if (thread >= top.threadCount) throw std::runtime_error("thread index out of range");
        if (words.size() * 8 > top.programCapacity) throw std::runtime_error("program exceeds configured fixture memory");
        uint64_t cycles = 0;
        auto step = [&]() {
            top.clock = 0;
            top.eval();
            if (plan.empty() && top.uartValid) std::cout << static_cast<char>(top.uartData) << std::flush;
            context.timeInc(1);
            top.clock = 1;
            top.eval();
            context.timeInc(1);
            ++cycles;
            if (context.gotFinish()) throw std::runtime_error("RTL finished before plan completed");
        };
        auto reg = [&](uint64_t t, uint64_t r) -> uint64_t {
            if (t >= top.threadCount || r > 31) throw std::runtime_error("register index out of range");
            top.threadIndex = t;
            top.registerIndex = r;
            top.eval();
            return top.registerValue;
        };
        auto dump = [&]() {
            for (uint64_t t = 0; t < top.threadCount; ++t) {
                std::cout << "thread " << t << " at cycle " << cycles << '\n';
                for (int r = 0; r < 32; ++r) {
                    auto value = reg(t, r);
                    std::cout << "x" << r << " = " << value << " (0x" << std::hex << value << std::dec << ")\n";
                }
            }
        };
        for (int i = 0; i < 10; ++i) step();
        top.reset = 0;
        top.loadValid = 1;
        top.loadData = words.size();
        for (auto word : words) {
            step();
            top.loadData = word;
        }
        for (int i = 0; i < 20; ++i) {
            step();
            top.loadData = 0;
        }
        step();
        top.loadValid = 0;
        step();
        cycles = 0;
        auto expect = [&](uint64_t t, uint64_t r, uint64_t value) {
            auto actual = reg(t, r);
            if (actual != value) {
                std::ostringstream message;
                message << "cycle " << cycles << ": thread " << t << " x" << r
                        << "=" << actual << ", expected " << value;
                throw std::runtime_error(message.str());
            }
            std::cout << "EXPECT cycle=" << cycles << " thread=" << t << " x" << r << "=" << value << '\n';
        };
        auto output = [&](uint64_t timeout, bool print, int expectedByte = -1) -> char {
            uint64_t n = 0;
            while (!top.uartValid && n < timeout) { step(); ++n; }
            if (!top.uartValid) throw std::runtime_error("UART timeout at cycle " + std::to_string(cycles));
            char c = top.uartData;
            if (expectedByte >= 0 && static_cast<unsigned char>(c) != expectedByte)
                throw std::runtime_error("UART byte=" + std::to_string(static_cast<unsigned char>(c)) + ", expected " + std::to_string(expectedByte));
            top.uartReady = 1;
            top.eval();
            if (print) std::cout << c << std::flush;
            step();
            top.uartReady = 0;
            top.eval();
            return c;
        };
        try {
            if (plan.empty()) {
                uint64_t n = 0;
                while (reg(thread, index) != expected && n < maxCycles) { step(); ++n; }
                expect(thread, index, expected);
            } else {
                std::ifstream actions(plan);
                if (!actions) throw std::runtime_error("cannot open plan: " + plan);
                std::string line;
                size_t action = 0;
                while (std::getline(actions, line)) {
                    ++action;
                    std::istringstream in(line);
                    std::string op;
                    in >> op;
                    auto arg = [&]() { std::string s; if (!(in >> s)) throw std::runtime_error("missing action argument"); return number(s); };
                    try {
                        if (op == "step") {
                            auto n = arg(); for (uint64_t i = 0; i < n; ++i) step();
                        } else if (op == "expect" || op == "wait" || op == "change") {
                            auto t = arg(), r = arg(), value = arg();
                            if (op != "expect") {
                                auto timeout = arg();
                                uint64_t n = 0;
                                while ((op == "change" ? reg(t, r) == 0 : reg(t, r) != value) && n < timeout) { step(); ++n; }
                                if (op == "change" && reg(t, r) == 0) throw std::runtime_error("register did not change");
                            }
                            expect(t, r, value);
                            if (op == "change") for (int i = 0; i < 3; ++i) step();
                        } else if (op == "ready") {
                            top.uartReady = arg(); top.eval();
                        } else if (op == "uart" || op == "any") {
                            auto timeout = arg(), print = arg();
                            auto value = op == "uart" ? arg() : 0;
                            if (value > 255) throw std::runtime_error("UART expectation must be a byte");
                            // checkForOutput checks the byte before getOutput consumes it.
                            output(timeout, print, op == "uart" ? static_cast<int>(value) : -1);
                        } else if (op == "search" || op == "suffix") {
                            auto timeout = arg(), print = arg(), limit = arg(), length = arg();
                            std::string text;
                            for (uint64_t i = 0; i < length; ++i) {
                                auto value = arg();
                                if (value > 255) throw std::runtime_error("UART search value must be a byte");
                                text += static_cast<char>(value);
                            }
                            size_t idx = 0;
                            std::string tail;
                            uint64_t n = 0;
                            for (; idx < text.size() && n < limit; ++n) {
                                char c = output(timeout, print);
                                if (op == "search") idx = c == text[idx] ? idx + 1 : 0;
                                else {
                                    tail += c;
                                    if (tail.size() > text.size()) tail.erase(0, 1);
                                    if (tail == text) idx = text.size();
                                }
                            }
                            if (idx != text.size()) throw std::runtime_error("UART search did not terminate within " + std::to_string(limit) + " characters");
                        } else if (op == "dump") dump();
                        else if (op == "stats") {
                            std::string path, prefix;
                            in >> std::quoted(path) >> std::quoted(prefix);
                            std::ofstream out(path, std::ios::app);
                            if (!out) throw std::runtime_error("cannot open statistics file");
                            out << prefix << "\"ipc\":[";
                            for (uint64_t t = 0; t < top.threadCount; ++t) {
                                if (t) out << ',';
                                out << std::setprecision(17) << static_cast<double>(reg(t, 6)) / static_cast<double>(reg(t, 5));
                            }
                            out << "]}\n";
                        } else if (op == "monitor") {
                            auto n = arg(), address = arg();
                            bool waiting = false;
                            for (uint64_t i = 0; i < n; ++i) {
                                if (!waiting && top.writeAddressValid && top.writeAddress == address) waiting = true;
                                if (waiting && top.writeDataValid) { std::cout << static_cast<char>(top.writeData) << std::flush; waiting = false; }
                                step();
                            }
                        } else throw std::runtime_error("unknown action: " + op);
                        std::string extra;
                        if (in >> extra) throw std::runtime_error("extra action argument: " + extra);
                    } catch (const std::exception &e) {
                        throw std::runtime_error("action " + std::to_string(action) + " (" + line + "): " + e.what());
                    }
                }
                if (!actions.eof() || actions.bad()) throw std::runtime_error("cannot read plan: " + plan);
                if (action == 0) throw std::runtime_error("plan contains no actions");
            }
        } catch (...) {
            dump(); top.final(); throw;
        }
        std::cout << "PASS completed at cycle " << cycles << " after loading\n";
        top.final();
        return 0;
    } catch (const std::exception &error) {
        std::cerr << "FAIL: " << error.what() << '\n';
        return 1;
    }
}
