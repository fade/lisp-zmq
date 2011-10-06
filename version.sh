#!/bin/sh
awk '/:version/ {print substr($2, 2, length($2)-2)}' zmq.asd
