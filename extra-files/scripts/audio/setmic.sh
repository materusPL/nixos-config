#!/usr/bin/env bash

# Script to fix mic on SoundBlasterX G6
# Source: https://askubuntu.com/questions/1123065/soundblaster-g6-microphone-input-doesnt-work 

exec >> /tmp/setmic.log 2>&1
echo "Script started at $(date)"

cardNumber=$(aplay -l|grep 'Sound BlasterX G6'|cut -d' ' -f 2 |tr -d ':')
amixer -c "$cardNumber" -q set "PCM Capture Source" "External Mic"

if [ $? -eq 0 ]; then
  echo "PCM Capture Source successfully changed to 'External Mic'"
else
  echo "Failed to configure PCM Capture source for Sound BlasterX G6"
fi

amixer -c "$cardNumber" -q sset 'Input Gain Control' 3

if [ $? -eq 0 ]; then
  echo "Input Gain Control set to 3"
else
  echo "Failed to activate mic Boost for Sound BlasterX G6"
fi