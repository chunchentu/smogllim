#!/usr/bin/bash
cvID=1
data_source="\"OJ\""
save_prefix="\".\""
RoutName="temp.out"


R CMD BATCH "--no-save --no-restore --args cvID=$cvID data_source=$data_source save_prefix=$save_prefix" run_sim_wrapper.R $RoutName

