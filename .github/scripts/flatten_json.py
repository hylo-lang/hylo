#!/usr/bin/env python

import functools as ft
import json
import sys

input = ft.reduce(lambda acc, i: acc + i, sys.stdin, "")
print(json.dumps(json.loads(input), separators=(',', ':')))
