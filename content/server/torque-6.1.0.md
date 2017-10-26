title: torque 6.1でarray jobが300しか実行されない
date: 2017-10-25

torqueのバージョンを上げると`qstat -q`でみたときに下記のように300しかでなくなった。

```
Queue            Memory CPU Time Walltime Node  Run Que Lm  State
---------------- ------ -------- -------- ----  --- --- --  -----
batch              --      --       --      --    0  300 --   E R
                                               ----- -----
                                                   0   300

```
http://docs.adaptivecomputing.com/torque/6-1-0/releaseNotes/Content/topics/releaseNotes/newFeatures.htm

> Job Arrays Now Only Partially Instantiated By Default
> 
> Job arrays are now only partially instantiated by default; additional idle jobs are added as the array is executed. This is controlled by the new idle_slot_limit server parameter.
>
>A new qsub -i option has also be added to set the slot limit for the job array being submitted. If set for a non-array job, it will be rejected. If the user requests an idle slot limit that exceeds the server parameter's default, the job will be rejected.

ということなので

`qmgr -c 'set server idle_slot_limit = 1000'`

のように適当な値を入れる必要がある。

qsubにも-i オプションが増えてるのでarray jobのコントロールがやりやすくなっている。

