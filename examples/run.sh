#!/bin/bash
KO='\E[1;31m'"*KO\033[1m\033[0m"
OK='\E[1;32m'"OK\033[1m\033[0m"

file=$1

args=`if test -f ${file}.args ; then echo ${file}.args; else echo "/dev/null"; fi`
(xargs ./${file} < ${args} > ${file}.out 2>&1 && echo -e ${OK}" ${file}" ) || echo -e ${KO}" ${file}"