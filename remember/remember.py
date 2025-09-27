import json
from pathlib import Path
from datetime import datetime
from colorama import Fore
import subprocess

def list_commands(data: dict):
    # return: True if change was made to data, otherwise False
    # (obviously always returns False)
    for key, value in data.items():
        print(Fore.CYAN + key + Fore.RESET + ": " + value)

    return False

def delete_command(data: dict):
    # return: True if change was made to data, otherwise False

    cmd_key = input("enter the name of the command you wish to remove: ")
    if cmd_key in data.keys():
        data.pop(cmd_key)
        print("done!")
        return True
    else:
        print(f"error: given value '{cmd_key}' doesn't exist. nothing was removed")
        return False

def rename_command(data: dict):
    # return: True if change was made to data, otherwise False

    cmd_key = input("enter the name of the command you wish to rename: ")
    if cmd_key not in data.keys():
        print(f"error: given value '{cmd_key}' doesn't exist")
        return False
    
    new_key = input("enter new name: ")
    value = data.pop(cmd_key)
    data[new_key] = value
    print("done!")

    return True

def save_command(data: dict):
    # return: True if change was made to data, otherwise False

    cmd = input("enter command to be saved: ")
    
    now = datetime.now() # current date and time
    now_str = now.strftime("%Y/%m/%d %H:%M:%S")

    data[now_str] = cmd
    print("done!")

    return True

def save_file(data: dict, path):
    with path.open('w') as f:
        json.dump(data, f, indent=4)

def load_data():
    p = Path(__file__).with_name('data.json')
    if p.exists():
        with p.open('r') as f:
            data = json.load(f)
    else:
        data = {}

    return data, p 

def execute_command(data: dict):
    cmd_key = input("enter the name of command to be executed: ")

    if cmd_key not in data.keys():
        print(f"error: given value {cmd_key} doesn't exist")

    subprocess.run(data[cmd_key], shell=True)

    print("done!")

def display_options(options: dict):
    for value in options.values():
        print(f"{value[0]}")

options =	{
    "l": ("(l)ist commands", list_commands),
    "s": ("(s)ave command", save_command),
    "r": ("(r)ename command", rename_command),
    "e": ("(e)xecute command", execute_command),
    "d": ("(d)elete command", delete_command),
    "h": ("(h)elp", display_options),
    "q": ("(q)uit", None)
}

def main():

    display_options(options)
    data, path = load_data()

    while True:
        usr_input = input("").lower()[0]

        if usr_input not in options.keys():
            print("error: invalid option")
            continue

        if usr_input == "q":
            return

        need_to_save = options[usr_input][1](data)
        if need_to_save: save_file(data, path)

if __name__ == "__main__":
    main()