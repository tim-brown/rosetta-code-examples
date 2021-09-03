#!/usr/bin/bash

task=$1

cat <<EOS
={{header|Racket}}=

{{trans|xxx}}

$(cat tasks/$task/README.md)

<lang racket>$(cat tasks/$task/$task.rkt)</lang>

{{out}}
EOS
cat <<EOS

<pre>$(racket tasks/$task/$task.rkt)</pre>
EOS
