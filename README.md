# Relational algebra parser relational-algebra-parser [![Build Status](https://travis-ci.com/jcavat/relational-algebra-parser.svg?branch=master)](https://travis-ci.com/jcavat/relational-algebra-parser)

## Summary 

A pedagogic parser aims to translate relational algebra into SQL written in Scala with [https://www.lihaoyi.com/fastparse/](Fastparse 2.x)


### How to 

Run

```
sbt run "sigma(age = 20)(Person)"
```

Bundle

```
sbt assembly
```

Run with java

```
java -cp target/scala
```
