-*- koshu -*-

|== DIR        : dir
|== DIR-TOTAL  : dir-total
|== FILE       : file
|== FILE-TOTAL : file-total

dir        : source DIR        /package /code /comment /dir
file       : source FILE       /package /code /comment /file /dir
dir-total  : source DIR-TOTAL  /package /code /comment
file-total : source FILE-TOTAL /package /code /comment

|== FILE-LONG -fore /rank /code /package :
  long-top 10

long-top : file
  | cut /comment
  | add /=code-minus ( - /code )
  | rank /rank -from 1 -order /=code-minus
  | keep /rank <= @'1
  | wipe
