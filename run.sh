#!/bin/bash

mono splitter/bin/Debug/splitter.exe bnf.xml process
mono bin/ldbnf/ldbnf.exe process
