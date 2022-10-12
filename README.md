602277113 이다운
# 10-12 7주차 실습

#==============================================
# 05-1: Geocoding 준비
#==============================================

# 1단계: 카카오 로컬 API키 발급받기
# develop kakao 
# REST API 키 확인

# 2단계 : 중복된 주소 제거
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
load( "./04_preprocess/04_preprocess.rdata") #실거래 불러오기
apt_juso <- data.frame(apt_price$juso_jibun) #주소컬럼만 추출
apt_juso <- data.frame(apt_juso[!duplicated(apt_juso), ]) #unique 주소 추출
head(apt_juso, 2) #추출결과 확인

#==============================================
# 05-2: 주소를 좌표로 변환하는 지오코딩
#==============================================

#1단계: 지오코딩 준비
add_list <- list() # 빈 리스트 생성
cnt <- 0          #카운팅 초기값 설정       
kakao_key = "926ab2d50967a3b4ffd4c9bc3d112a90" # 인증키

#2단계 : 지오코딩
#install.packages('httr')
#install.packages('RJSONIO')
#install.packages('data.table')
#install.packages('dplyr')
library(httr)
library(RJSONIO)
library(data.table)
library(dplyr)

for(i in 1:nrow(apt_juso)){
  #---# 에러 예외처리 구문 시작
  tryCatch(
    {
      #---# 주소로 좌표값 요청
      lon_lat <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
                     query = list(query = apt_juso[i,]),
                     add_headers(Authorization = paste0("KaKaoAK ", kakao_key)))
      coordxy <- lon_lat %>% content(as = 'text') %>% RJSONIO::fromJSON()
      cnt = cnt + 1
      add_list[[cnt]] <- data.table(apt_juso = apt_juso[i,],
                                    coord_x = coordxy$documents[[1]]$x,
                                    coord_y = coordxy$documents[[1]]$y)
      message <- paste0("[",i,"/",nrow(apt_juso),"]번째(",
                      round(i/nrow(apt_juso)*100,2),"%) [",apt_juso[i,], "] 지오코딩 중입니다:
                      X=", add_list[[cnt]]$coord_x, " / Y=", add_list[[cnt]]$coord_y)
      cat(message, "\n\n")
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
  )
}

# 3단계 : 지오 코딩 결과 저장
juso_geocoding<- rbindlist(add_list) #리스트를 데이터프레임 변환
juso_geocoding$coor_x <- as.numeric(juso_geocoding$coord_x) # 좌표값 숫자형 변환
juso_geocoding$coor_y <- as.numeric(juso_geocoding$coord_y)
juso_geocoding <- na.omit(juso_geocoding) # 결측치 제거
dir.create("./05_geocoding") #새로운 폴더 생성
save(juso_geocoding, file ="./05_geocoding/05_juso_geocoding.rdata") #저장
write.csv(juso_geocoding,"./05_geocoding/05_juso_geocoding.csv")

# 10-05 6주차 실습

# 04-1 불필요한 정보 지우기

# 1단계: 데이터 불러오기
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
options(warn=-1)

load("./03_integrated/03_apt_price.rdata")
head(apt_price, 3)

# 2단계: 공백제거
# NA제거
table(is.na(apt_price)) # NA가 있는지 없는지 확인

apt_price <- na.omit(apt_price)
table(is.na(apt_price)) # NA가 잘 제거 됬는지 확인

# 공백 제거
head(apt_price$price, 3)

# install.packages("stringr")
library(stringr)
apt_price <- as.data.frame(apply(apt_price,2,str_trim)) #apply([적용 테이블], [1:raw / 2:col],[적용 함수] ])

#
# 04-2 항목별 데이터 다듬기

# 1단계 : 매매 연월일 만들기
install.packages("lubridate")
library(lubridate)
install.packages("dplyr")
library(dplyr)

apt_price <- apt_price %>% mutate(ymd=make_date(year, month, day))
apt_price$ym <- floor_date(apt_price$ymd, "month")
head(apt_price,3)

# 2단계 : 매매가 변환
head(apt_price$price, 3)

apt_price$price <- apt_price$price %>% sub(",","",.) %>% as.numeric()

# 3단계 : 주소 조합하기
head(apt_price$apt_nm, 100) #확인

apt_price$apt_nm <- gsub("\\(.*","", apt_price$apt_nm) #필요없는 ()안의 주소 삭제

loc <- read.csv("./sigun_code.csv", fileEncoding = "UTF-8")

apt_price <- merge(apt_price, loc, by = 'code')
apt_price$juso_jibun <- paste0(apt_price$adde_2, " ",apt_price$dong_nm, " ", apt_price$jiban, " ", apt_price$apt_nm)

head(apt_price, 5)

# 4단계 : 건축연도, 전용면적 변환
head(apt_price$con_year,5)

# 건축연도 변환
apt_price$con_year <- apt_price$con_year  %>% as.numeric()

# 전용면적 변환
head(apt_price$area, 5)

apt_price$area <- apt_price$area %>% as.numeric() %>% round(0)

#
# 5단계 : 평당 매매가 계산

apt_price$py <- round(((apt_price$price/apt_price$area)*3.3), 0)
head(apt_price$py, 5)

#
# 6단계 : 층수 변환
min(apt_price$floor)

apt_price$floor <- apt_price$floor %>% as.numeric() %>% abs()

# 카운트 추가
apt_price$cnt <- 1
head(apt_price, 5)



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
