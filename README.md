602277113 이다운

# 09-28 5주차 실습

# 03-2 요청 목록 생성
##---------------------

## 1단계 요청목록 만들기
url_list <- list()
cnt <- 0

## 2단계: 요청목록 채우기
for (i in 1:nrow(loc)) { # 25개 자치구
  for (j in 1:length(datelist)) { # 4개월
    cnt <- cnt + 1
    # 요청목록 채우기 25*4 = 100개
    url_list[cnt] <- paste0("	http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade"
                            , loc[i, 1],   # 지역코드
                            "&DEAL_YMD=", datelist[j],   # 계약 년월
                            "&numOfLows=", 100,   # 한번에 가져올 최대 자료 수
                            "&serviceKey=", service_key) # Encoding 인증키
  }
  Sys.sleep(0.1) # 0.1초간 멈춤
  msg <- paste0("[", i, "/", nrow(loc), "]", loc[i, 3], "의 크롤링 목록이 생성됨 => 총 [", cnt, "] 건") # 알림 메시지
  cat(msg, "\n\n")
}

## 3단계: 요청 목록 확인
length(url_list) # 목록 개수 
browseURL(paste0(url_list[i])) #브라우저에 뛰워서 정상동작 확인


# 03-3 크롤러 제작
#------------------

## 1단계: 임시 저장 리스트 생성
## install.packages("XML")
## install.packages("data.table")
## install.packages("stringr")
library(XML)
library(data.table)
library(stringr)

raw_data <- list() # XML 파일 저장소
root_Node <- list() # 거래 내역 추출 데이터 임시 저장
total <- list() # 거래 내역 정리 데이터 임시 저장
dir.create("02_raw_data") # 새로운 디렉토리 생성

## 2단계: 자료 요청 및 응답 받기
for(i in 1:length(url_list)){
  raw_data[[i]] <- xmlTreeParse(url_list[i], useInternalNodes = TRUE, encoding ="UTF-8")
  root_Node[[i]] <- xmlRoot(raw_data[[i]])

## 3단계 : 전체 거래 건수 확인
  items <- root_Node[[i]][[2]][['items']] #전체 거래내역 추출
  size <- xmlSize(items)
  
## 4단계 : 개별 거래 내역 추출
  item <- list()
  item_temp_dt <- data.table()
  Sys.sleep(.1)
  for(m in 1:size){
    item_temp <- xmlSApply(items[[m]], xmlValue)
    item_temp_dt <- data.table(year = item_temp[4],
                               month = item_temp[7],
                               day = item_temp[8],
                               price = item_temp[1],
                               code = item_temp[12],
                               dong_nm = item_temp[5],
                               jiban = item_temp[11],
                               con_year = item_temp[3],
                               apt_nm = item_temp[6],
                               area = item_temp[9],
                               floor = item_temp[13])
    item[[m]] <- item_temp_dt
    }
  apt_bind <- rbindlist(item)
  
## 5단계 : 응답내역 저장
  region_nm <- subset(loc, code==str_sub(url_list[i],115,119))$addr_1
  month <- str_sub(url_list[i],130,135)
  path <- as.character(paste0("./02_raw_data/", region_nm, "_", month, ".csv"))
  write.csv(apt_bind, path)
  msg <- paste0("[", i, "/", length(url_list), "] 수집한 데이터를 [", path, "]에 저장 합니다.")
  cat(msg, "\n\n")
}

# 03-4 자료 통합하기

## 1단계 : csv파일 통합하기
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
files <- dir("./02_raw_data")

install.packages("plyr")
library(plyr)
apt_price <- ldply(as.list(paste0("./02_raw_data/", files)), read.csv)
tail(apt_price, 2)


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
