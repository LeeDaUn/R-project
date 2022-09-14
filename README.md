602277113 이다운

#이지스퍼블리싱 자료실 2번째 소스파일 다운로드

#------------------------------------
# 03-1 크롤링 준비 : 무엇을 준비할까?
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
