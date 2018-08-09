#!/usr/bin/env python3

import sys
import re
import configparser


def subst(text, key, val):
    return text.replace('{' + key.upper() + '}', val)


def gen(spec_template, spec_name, rules):
    genspec = subst(spec_template, 'module', spec_name.upper())
    genspec = subst(genspec, 'rules', rules)
    print(genspec)

if __name__ == '__main__':
    if len(sys.argv) < 3:
        print("usage: <spec-template> <spec_name> <rules>")
        sys.exit(1)
    spec_template = open(sys.argv[1], "r").read()
    rules = open(sys.argv[3], "r").read()
    gen(spec_template, sys.argv[2], rules)
