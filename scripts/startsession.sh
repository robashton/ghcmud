#!/bin/bash
player_id=$1

echo "Setting up player $player_id"
curl "http://localhost:3000/login/$player_id"
echo ""
