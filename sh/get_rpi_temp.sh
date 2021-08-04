#!/usr/bin/env bash

vcgencmd measure_temp | egrep -o '[0-9]*\.[0-9]*'
