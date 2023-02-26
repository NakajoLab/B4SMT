import json
import csv

with open("stats.jsonl", "r") as f, open("stats.csv", "w") as c:
    writer = csv.writer(c)
    labels = ["threads","executor","decoders","maxCommitCount","tagWidth","lsqWidth"]
    writer.writerow(labels+[ f"ipc{i}" for i in range(8)])
    for l in f:
        l = json.loads(l)
        writer.writerow(list(map(lambda x:l[x],labels))+[ i for i in l["ipc"]])
