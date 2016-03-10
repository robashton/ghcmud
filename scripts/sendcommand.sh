#!/bin/bash
player_id=$1
cmd=$2

cmd_body="command=""$cmd"""

curl -v -X POST -F "$cmd_body" "http://localhost:3000/command/$player_id"
echo ""

