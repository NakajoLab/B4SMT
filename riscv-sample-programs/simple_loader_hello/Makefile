PROGRAMNAME := loader-hello
SOURCES := entry.S

TOOL_PREFIX := riscv64-none-elf-
export CC := $(TOOL_PREFIX)gcc
export OBJCOPY := $(TOOL_PREFIX)objcopy
export OBJDUMP := $(TOOL_PREFIX)objdump
export CFLAGS := -nodefaultlibs -nostdlib -march=rv64ic_zicsr -mabi=lp64 -no-pie -static -g -O2
export LINKER_SCRIPT := linker.ld

CFLAGS += -T$(LINKER_SCRIPT)

.PHONY:clean
all: $(PROGRAMNAME).hex $(PROGRAMNAME).dump $(PROGRAMNAME).text64.hex


$(PROGRAMNAME).o: $(SOURCES) $(LINKER_SCRIPT)
	$(CC) $(CFLAGS) $(SOURCES) -o $@

$(PROGRAMNAME).binary: $(PROGRAMNAME).o
	$(OBJCOPY) -O binary $< $@

$(PROGRAMNAME).hex: $(PROGRAMNAME).binary
	od -An -t x1 $< -w1 -v | tr -d " " > $@

$(PROGRAMNAME).text64.hex: $(PROGRAMNAME).binary
	od -An -t x8 $< -w8 -v | tr -d " " > $@

$(PROGRAMNAME).dump: $(PROGRAMNAME).o
	$(OBJDUMP) -d -j .text -j .data -j .bss -j .rodata $< -M no-aliases -M numeric > $@

clean:
	$(RM) *.hex *.binary *.o *.dump