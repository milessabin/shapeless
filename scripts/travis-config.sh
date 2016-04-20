#!/bin/bash

#travis encrypt -r milessabin/shapeless SONATYPE_USERNAME=milessabin --add env.global
#travis encrypt -r milessabin/shapeless SONATYPE_PASSWORD=Byugdelgoi --add env.global
travis encrypt -r milessabin/shapeless TEST_VAR=wibble --add env.global
