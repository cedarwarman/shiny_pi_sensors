#!/usr/bin/env bash

vcgencmd measure_temp | egrep -o '[0-9]*\.[0-9]*' | tee /home/pi/shiny-server/apps/pi_sensor/temperature.txt /home/pi/shiny-server/apps/pi_sensor_home/temperature.txt
