import json
from pathlib import Path
from datetime import datetime
import subprocess

def main():

    arg = input("Provide command to be saved: ")

    now = datetime.now() # current date and time
    now_str = now.strftime("%Y/%m/%d %H:%M:%S")

    p = Path(__file__).with_name('data.json')

    if p.exists():
        with p.open('r') as f:
            data = json.load(f)
    else:
        data = {}

    data[now_str] = arg

    with p.open('w') as f:
        json.dump(data, f, indent=4)

    subprocess.run(arg, shell=True)

    return

if __name__ == "__main__":
    main()