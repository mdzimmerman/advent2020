#!/bin/sh

day="$1"

sbt "runMain org.triclinic.$day"
