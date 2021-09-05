#!/bin/bash

task=$(basename $1)

cat <<EOS
=={{header|Racket}}==

{{trans|xxx}}

$(cat tasks/$task/README.md)

<lang racket>$(cat tasks/$task/$task.rkt)</lang>

{{out}}
EOS
cat <<EOS

<pre>$(cd tasks/$task; racket $task.rkt)</pre>
EOS
