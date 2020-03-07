import json

def readCurrent() :
    with open("transactions.json") as f: 
        return json.loads(f.read())

with open("commandlog.jsonl", "a") as out:    
    for transaction in reversed(readCurrent()):
        data = {"timestamp": transaction["date"],
                "command": {"type": "Add",
                            "value": transaction}}
        json.dump(data, out)
        out.write("\n")