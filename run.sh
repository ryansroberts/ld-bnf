#!/bin/bash

mono splitter/bin/Release/splitter.exe bnf.xml process drug
mono bin/ldbnf/ldbnf.exe --xmldirectory process --outputdirectory process
