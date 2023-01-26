#!/bin/bash

sudo yum install python3
sudo yum install tmux

cd pk-sky/Code/bisp_approach/03_analysis
python3 -m venv env
source env/bin/activate
pip3 install -r requirements.txt

python3 run_grid.py
