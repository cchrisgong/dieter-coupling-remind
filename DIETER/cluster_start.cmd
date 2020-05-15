#!/bin/bash
#SBATCH --qos=priority
#SBATCH --time=01:00:00
#SBATCH --mail-type=END

gams DIETER_v1.0.2.gms -errmsg=1 -pw=200 -ps=0
