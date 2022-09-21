602277113 이다운

# 09-21 4주차 실습 

## 1단계 요청목록 만들기
url_list <- list()
cnt <- 0

## 2단계 요청목록 채우기
for(i in 1:nrow(loc)){
  for(j in 1:length(datelist)){
    cnt <- cnt + 1
    url_list[cnt] <- paste("http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade",
                           "LAWD_CD = ", loc[i,1],
                           "&DEAL_YMD = ", datelist[j],
                           "&numOfLows = ", 100,
                           "&serviceKey = ", service_key)
  }
  Sys.sleep(0.1)
  msg<-paste("[", i, "/", nrow(loc), "]", loc[i,3], "의 크롤링 목록이 생성됨 => 총 [", cnt, "] 건")
  cat(msg,"\n\n")
}

### 결과값

[ 1 / 25 ] 종로구 의 크롤링 목록이 생성됨 => 총 [ 12 ] 건 

[ 2 / 25 ] 중구 의 크롤링 목록이 생성됨 => 총 [ 24 ] 건 
.
.
.
[ 24 / 25 ] 송파구 의 크롤링 목록이 생성됨 => 총 [ 288 ] 건 

[ 25 / 25 ] 강동구 의 크롤링 목록이 생성됨 => 총 [ 300 ] 건 


## 3단계 요청 목록 확인
length(url_list)
browseURL(paste0(url_list[i]))

#--------------------------
## 03-3 크롤러 제작
#--------------------------

## 1단계 :  임시 저장 리스트 생성
###install.packages("XML")
###install.packages("data.table")
###install.packages("stringr")

library(XML)
library(data.table)
library(stringr)

raw_data <- list()
root_Node <- list()
total <- list()
dir.create("02_raw_data")

# 09-14 3주차 실습 

#이지스퍼블리싱 자료실 2번째 소스파일 다운로드

#------------------------------------
## 03-1 크롤링 준비 : 무엇을 준비할까?
#------------------------------------

#---#  [1단계: 작업폴더 설정하기]

#install.packages("rstudioapi") #rstudioapi설치
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()

#---# [2단계: 수집 대상지역 설정]

loc <- read.scv("./sigun_code.scv", fileEncoding="UTF-8")
loc$code <- as.character(loc$code) #행정구역명 문자 변환
head(loc, 2) #확인

#---# [3단계: 수집 기간 설정]

datelist <- seq(from = as.Date('2021-01-01'),  # 시작
                to   = as.Date('2021-12-31'),  # 종료
                by   = '1 month')              # 단위
datelist <- format(datelist, format = '%Y%m')  # 형식변환 
datelist[1:3]           # 확인
