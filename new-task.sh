#!/bin/bash

echo -n "task name:"
read task_name

echo -n "wikipedia url:"
read wikipedia_url

mkdir tasks/$task_name

cat > tasks/$task_name/README.md <<EOS 
# ${task_name}

This is the Racket implementation of the: [$task_name](https://rosettacode.org/wiki/${task_name}) task.

Wikipedia reference: (${wikipedia_url}))
EOS

cat > tasks/$task_name/${task_name}.rkt <<EOS
#lang racket/base

(define ($task_name)
  (error "not implemented"))

(module+ main
  ($task_name))

(module+ test
  (require rackunit)
  (check-not-exn $task_name))
EOS
