#!/usr/bin/env python
"""
Usage:
  turing_machine.py FILENAME INITIAL
  turing_machine.py FILENAME INITIAL [-r|--rate] [RATE]

Options:
  -h --help
  -r --rate  specify delay rate
"""
import time
import copy
import sys
from docopt import docopt

class TuringMachine(object):
    def __init__(self, filename, initial, rate=.05):
        self.commands = self.parse_file(filename)
        self.tape = list(f"_{initial}_")
        self.rate = float(rate)
        self.place = 1
        self.state = "0"
        self.key = (self.state, self.tape[self.place])
        self.wild_key = (self.state, "*")

    def parse_file(self, filename):
        with open(filename) as f:
            lines = map(str.strip, f.read().splitlines())
            lines = [line for line in lines if not line.startswith(';')]
            lines = map(lambda x: x.split(';')[0].strip().split(' '), lines)
            return dict([(tuple(i[:2]),tuple(i[2:])) for i in lines])

    def run(self):
        while (self.key in self.commands
               or self.wild_key in self.commands
               and not self.key[0].startswith("halt")):
            time.sleep(self.rate)
            newchar, action, newstate = self.commands.get(self.key) or self.commands[self.wild_key]
            self.tape[self.place] = (newchar if newchar != '*' else self.tape[self.place])

            if action == "l":
                self.place = self.place - 1
                if self.place == -1:
                    self.tape.insert(0,"_")
                    self.place = 0
            elif action == "r":
                self.place = self.place + 1
                if self.place == len(self.tape):
                    self.tape.append("_")
            self.state = newstate
            # print(self.state)
            self.key = (self.state, self.tape[self.place])
            self.wild_key = (self.state, '*')
            tape_viz = copy.deepcopy(self.tape)
            tape_viz[self.place] = f'\033[7m{tape_viz[self.place]}\033[0m'
            print(''.join(tape_viz), end=('\r' if not self.key[0].startswith("halt") else ' '))
        else:
            if not self.key[0].startswith("halt"):
                print(f"No rule for state '{self.state}' and char '{self.tape[self.place]}'.")
            else:
                print('...Done!')

if __name__ == "__main__":
    arguments = docopt(__doc__)
    # print(arguments)
    machine = TuringMachine(arguments['FILENAME'], arguments['INITIAL'], (arguments['RATE'] or .05))
    machine.run()
