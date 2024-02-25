library(tidyverse)
library(data.table)
library(scales)
library(sampling)
library(future)
library(future.apply)
library(furrr)
library(stringr)
library(tidytext)
library(RcppMeCab)
library(KoNLP)


rm(list=ls())

file_path <- "C:/Users/jeong/OneDrive/바탕 화면"
file_path2 <- "C:/Users/jeong/OneDrive/crema/project/category_sales_analytics/data_extract"

setwd(file_path)


fa <- fread(file.path(file_path,"fashion_categories.csv"),encoding = "UTF-8")
mallid <- fread(file.path(file_path,"fashion_mall_list.csv"),encoding = "UTF-8")
#고객사 필터
mlist <- unique(mallid$brand_code)

pro <- fread(file.path(file_path2,"products_musinsa.csv"),encoding = "UTF-8")
pro1 <- fread(file.path(file_path2,"products_uni.csv"),encoding = "UTF-8")
pro1 <- pro1 %>% mutate(brand_code = "cre1.ma") %>% select(colnames(pro)) %>% 
  mutate_at(vars(brand_code,product_code,product_name,image_source_url,created_at),as.character) %>%
  mutate_at(vars(product_id,org_price_cents,final_price_cents),as.integer) %>%
  mutate_at(vars(shop_builder_created_at),as.logical)

pro <-  rbind(pro,pro1)

# pro <- pro %>% filter(brand_code %in% mlist)

pro_cate <- fread(file.path(file_path,"product_categories.csv"),encoding = "UTF-8")
# pro_cate <- pro_cate %>% filter(brand_code %in% mlist)

pro_cate2 <- fread(file.path(file_path,"brand_product_categorizations.csv"),encoding = "UTF-8")
# pro_cate2 <- pro_cate2 %>% filter(brand_code %in% mlist)







#카테고리 사전
falist <- as.list(fa$category)



# 카테고리 이름 테이블
# pro_cate2 <- pro_cate2 %>% filter(brand_code %in% mlist)
pro_cate2 <- pro_cate2 %>% left_join(pro_cate %>% select(brand_code,id,name) %>% rename(product_category_id=id) ,by=c("brand_code","product_category_id"))
pro_cate2 %>% glimpse
rm(list=ls()[7])

#카테고리 문자 숫자 외 제거, 대소문자 구분 제거 카테고리 사전 필터
pro_cate2$name <- gsub("\n|\r","",pro_cate2$name)
pro_cate2$name <- gsub("[^ \uAC00-\uD7A30a-zA-Z]","",pro_cate2$name)
pro_cate2$name <- tolower(pro_cate2$name)
notlist <- as.list("사용","노출","미노출","사용불가","사용종료")
pro_cate2 <- pro_cate2 %>% filter(  !str_detect(notlist,pro_cate2$name) )
pro_cate2 <- pro_cate2 %>% group_by(brand_code,product_id) %>% summarise(name=paste0(name,collapse=","))

# 상품에 카테고리 이름 붙이기
pro <- pro  %>% select(brand_code,product_id,product_name) %>%
  left_join(pro_cate2,by=c("brand_code","product_id"))
pro %>% glimpse
rm(list=ls()[8])



# 상품 문자 숫자 외 제거, 대소문자 구분 제거 카테고리 사전 필터
pro$product_name <- gsub("\n|\r","",pro$product_name)
pro$product_name <- gsub("[^ \uAC00-\uD7A30a-zA-Z]","",pro$product_name)
pro$product_name <- tolower(pro$product_name)



########################################################## 이름 전처리 추가
pro$product_name <- gsub("t shirts","tshirts",pro$product_name)
pro$product_name <- gsub("멀티","",pro$product_name)
pro$product_name <- gsub("슬링백 힐","슬링힐",pro$product_name)
pro$product_name <- gsub("setup","",pro$product_name)
pro$product_name <- gsub("반팔 t","반팔t",pro$product_name)
pro$product_name <- gsub("라운드 t","라운드t",pro$product_name)
pro$product_name <- gsub("boot cut","bcut",pro$product_name)
pro$product_name <- gsub("flat shoes","flatshoes",pro$product_name)
pro$product_name <- gsub("긴팔 t","긴팔t",pro$product_name)
pro$product_name <- gsub("카라 t","카라t",pro$product_name)
pro$product_name <- gsub("폴라 t","폴라t",pro$product_name)
pro$product_name <- gsub("니트 t","니트t",pro$product_name)
pro$product_name <- gsub("크롭 셔츠","크셔츠",pro$product_name)
pro$product_name <- gsub("크롭 티","크롭티",pro$product_name)
pro$product_name <- gsub("크롭 t","크롭t",pro$product_name)


mall1 <- c("merryaround.co.kr","holicholic.com","uptownholic.com")
pro[brand_code %in% mall1,]$product_name <- gsub("jk","jacket",pro[brand_code %in% mall1,]$product_name)
pro[brand_code %in% mall1,]$product_name <- gsub("cd","cardigan",pro[brand_code %in% mall1,]$product_name)
pro[brand_code %in% mall1,]$product_name <- gsub("nb","knit",pro[brand_code %in% mall1,]$product_name)
pro[brand_code %in% mall1,]$product_name <- gsub("sl","slacks",pro[brand_code %in% mall1,]$product_name)
pro[brand_code %in% mall1,]$product_name <- gsub("bl","blouse",pro[brand_code %in% mall1,]$product_name)
pro[brand_code %in% mall1,]$product_name <- gsub("sk","skirt",pro[brand_code %in% mall1,]$product_name)
pro[brand_code %in% mall1,]$product_name <- gsub("mt","맨투맨",pro[brand_code %in% mall1,]$product_name)
pro[brand_code %in% mall1,]$product_name <- gsub("bby","coat",pro[brand_code %in% mall1,]$product_name)
pro[brand_code %in% mall1,]$product_name <- gsub("pt","pants",pro[brand_code %in% mall1,]$product_name)
pro[brand_code %in% mall1,]$product_name <- gsub("sh","shoes",pro[brand_code %in% mall1,]$product_name)
pro[brand_code %in% mall1,]$product_name <- gsub("ct","coat",pro[brand_code %in% mall1,]$product_name)
pro[brand_code %in% mall1,]$product_name <- gsub("jp","jumper",pro[brand_code %in% mall1,]$product_name)
pro[brand_code %in% mall1,]$product_name <- gsub("kn","knit",pro[brand_code %in% mall1,]$product_name)

pro <- pro %>% rename(category_name=name)
pro$category_name[is.na(pro$category_name)] <- 0
pro %>% is.na %>% colSums()



############################################################ 카테고리 매칭 리스트
others <- c("래쉬가드","rashguard","잠옷","swimwear","트레이닝복","운동복","요가복","수영복","모니키니","비키니","파자마","로브","home wear","pajama","swimsuit")
non <- c("개인결제","입니다","샘플")
set <- c("세트","set","투피스","pack","팩 ","수트","슈트","suit")
acc <- c("acc","악세사리","악세서리","장갑","패션소품","양말","쥬얼리","주얼리","네크리스","넥워머","귀걸이","지갑","socks","bracelet","muffler","마스크",
         "파우치","핸드폰케이스"," 케이스","머니클립","스트랩","프로케이스","암커버","토시","glove","scarf","집게핀","헤어끈","우산","키링","리본핀","반다나",
         "링$","목걸이","팔찌","발찌","반지","벨트","헤어밴드","브로치","시계","머플러","목도리","스카프","안경","선글라스","월렛","hairband","타이$","핸드워머",
         "머리띠","헤어핀","곱창","belf","earring","pin$","gloves","삭스","이어링","wallet","pouch","chain","tie$","case","악세러리","necklace","ring","케이스",
         "belt","iphone","strap","sunglasses","넥타이","watch","선글라스","가모","pendant","헤어")
hat <- c("버킷햇","모자","볼캡","벙거지","베레모","썬캡","비니","cap","캡$","햇","버켓","hat","beanie")
shoes <- c("잡화","스니커즈","구두","힐$","로퍼","슈즈","슬리퍼","부츠","샌들","쪼리","캔버스","플립플랍","옥스퍼드","운동화","슬립온","boots","힐 ","shoe","워커",
           "하이힐","미들힐","플랫","sandal","슬링힐","sneaker","신발","flatshoes","loafer","heel"," toe","ankle","단화")
bag <- c("가방","클러치","백팩","백$","힙색", "슬링백", "메신저백", "토트백", "크로스백", "버켓백", "쇼퍼백","보스턴백","숄더백","bag","백 ","clutch","캐리어")
innerwear <- c("이너웨어","언더웨어","브라$","팬티","속바지","속치마","드로즈","누브라","브라렛","브라탑","내의","브라 ",
               "속옷","보정","inner","underwear","bra$","brief")
pants <- c("팬츠","슬렉스","슬랙스","바지","조거","부츠컷","테이퍼드","스키니","pants","jeans","pant","하의",
           "shorts","쇼츠","숏츠","데님","slacks","denim","slacks","jean","trousers","bcut")
onepiece <- c("원피스","드레스","점프수트","점프슈트","ops","onepiece","one-piece","dress","jumpsuit")
leggings <- c("레깅스","래깅스","스타킹","stockings","leggings")
skirt <- c("스커트","치마","skirt","skirts")
tshirt <- c("티셔츠","tshirt","t-shirt","tee","슬리브","티$","반팔티","칠부티","폴라티","반팔 t","크롭 t","반팔 티","라운드t","sleeve","긴팔t","카라t","tank",
            "크롭t","크롭티","crewneck","크루")
knitsweater <- c("니트","스웨터","knit","sweater","터틀넥","풀오버","니트티","폴라티","pullover","폴라t","니트t")
padding <- c("패딩","푸퍼","padding","down","다운")
coat <- c("코트","coat","트렌치")
cardigan <- c("가디건","cardigan","카디건")
jacket <- c("재킷","자켓","jacket","blazer","블레이저","저지")
sweatshirt <- c("맨투맨","후드","후디","스웨트셔츠","스웻셔츠","후드티","hood","sweatshirts","hoodie","sweat")
shirtblouse <- c("블라우스","셔츠","남방","blouse","^shirt"," shirts"," shirt")
topetc <- c("나시","상의","top","조끼","탑","민소매","슬리브리스","vest","뷔스티에","크롭탑","sleeveless","크셔츠")
outeretc <- c("무스탕","점퍼","바람막이","jumper","아우터","outer","플리스","후리스","아노락","브레이커","mustang","야상","파카","parka")
deplist <- list(jacket=jacket,padding=padding,coat=coat,cardigan=cardigan,outeretc=outeretc,shirtblouse=shirtblouse,tshirt=tshirt,knitsweater=knitsweater,sweatshirt=sweatshirt,topetc=topetc,pants=pants,skirt=skirt,leggings=leggings,onepiece=onepiece,innerwear=innerwear,hat=hat,bag=bag,shoes=shoes,acc=acc,set=set,non=non,others=others
)



#병렬 코어 선택
availableCores()
plan(multisession, workers = 10)

pro_test <- pro
rm(list=ls()[grep("pro",ls())][1])


leng <- length(pro_test$product_id)
cnum <- pro_test$category_name


plist <- ""
clist <- ""
flist <- ""

options(future.globals.maxSize = 16000 * 1024^2) 

#카테고리매칭
clist <-  future_map(1:leng, function(j)
  {
      lc <- c()
      for (i in 1:length(deplist)){
        c <- sum(ifelse(str_detect(cnum[j],deplist[[i]]),1,0))
        lc[i] <- c
      }
      clist <- paste(names(deplist)[which(lc==max(lc) & max(lc) != 0)],collapse = ",")
  })


# 연산 확인 
i = 0
while(T){
  cat(paste0(i, ' now running \n'))
  Sys.sleep(1)
  i = i + 1
  if(resolved(clist)){
    cat('Calculate done! \n')
    break
  }
}

pro_test$category_name2 <- ""
pro_test$category_name2 <- unlist(clist)

rm(list=ls()[grep("clist",ls())])
rm(list=ls()[grep("cnum",ls())])


# fwrite(pro_test,"protest1.csv")
# pro_test <- fread(file.path(file_path,"protest1.csv"),encoding = "UTF-8")




pnum <- pro_test$product_name
#상품명매칭
plist <-  future_map(1:leng, function(j)
  {
    lp <- c()
    for (i in 1:length(deplist)){
      p <- if(sum(str_locate(pnum[j],deplist[[i]])[,2],na.rm=T)==0)
      {sum(str_locate(pnum[j],deplist[[i]])[,2],na.rm=T)}
      else if(sum(str_locate(pnum[j],deplist[[1]])[,2],na.rm=T)-sum(str_locate(pnum[j],deplist[[i]])[,1],na.rm=T)==0)
      {sum(str_locate(pnum[j],deplist[[i]])[,2],na.rm=T)-1}else{sum(str_locate(pnum[j],deplist[[i]])[,2],na.rm=T)}
      lp[i] <- p
    }
    plist <- paste(names(deplist)[which(lp==max(lp) & max(lp) != 0)],collapse = ",")
  })



pro_test$product_name2 <- ""
pro_test$product_name2 <- unlist(plist)



fwrite(pro_test,"protest2.csv")
# pro_test <- fread(file.path(file_path,"protest1.csv"),encoding = "UTF-8")


  
#최종 카테고리

flist <-  future_map(1:leng, function(j)
  {
      if (pro_test$category_name2[j] == "" & pro_test$product_name2[j] == ""){
        flist <- "non"
      } else if (pro_test$product_name2[j] == "" & pro_test$category_name2[j] != "") {
        if (grepl(",",pro_test$category_name2[j])){
          flist <- strsplit( pro_test$category_name2[j],split=",")[[1]][1]
        }else{
          flist <- pro_test$category_name2[j]
        }
      } else{
        if (grepl(",",pro_test$product_name2[j])){
          flist <- strsplit( pro_test$product_name2[j],split=",")[[1]][length(strsplit(pro_test$product_name2[j],split=",")[[1]])]
        }else{
          flist <- pro_test$product_name2[j]
        }
      }
  })
  
# 연산 확인 
i = 0
while(T){
  cat(paste0(i, ' now running \n'))
  Sys.sleep(1)
  i = i + 1
  if(resolved(flist)){
    cat('Calculate done! \n')
    break
  }
}

pro_test$final <- ""
pro_test$final <- unlist(flist)

  
# pro_test <- 
# pro_test %>% glimpse %>% select(brand_code,product_id,final) %>% rename(categry_2depth=final) %>% 
# left_join(brand) %>% mutate(mallid_pid=paste0())


getwd()
fwrite(pro_test,"test.csv")
gc()
options(future.fork.multithreading.enable = FALSE)










jplist <- c(".jp","jp.")


table <- fread(file.path(file_path,"test.csv"),encoding = "UTF-8")
table <- table[table$product_name!="",]
table <- table[!grep(jplist, table$brand_code),]


#통계확인
unique(table$brand_code)
length(unique(table$brand_code))
total_perc <- table %>% group_by(final) %>% summarise(count=n())
#전체 카테고리 비중
total_perc1 <- total_perc %>% mutate(perc=percent(count/sum(total_perc$count), accuracy = 0.1))

mall_perc <- table %>% group_by(brand_code) %>% summarise(total_count=n()) 

mall_perc1 <- table %>% group_by(brand_code,final) %>% summarise(count=n()) %>% left_join(mall_perc ,by="brand_code") %>%
  mutate(perc=count/total_count)
#몰별 카테고리 비중 중 미 분류 22% 이상 몰
mall_f <- mall_perc1 %>% filter(final=="non",perc >= 0.22) %>% mutate(perc=percent(perc, accuracy = 0.1))
mall_f$brand_code

avg <- mall_perc1 %>% group_by(brand_code) %>% summarise(max=max(perc))
mall_g <- mall_perc1 %>% filter(perc >= mean(avg$max),final!="non") %>% mutate(perc=percent(perc, accuracy = 0.1))


# 빈도수 확인

table <- table[table$final=="non",c(3)]

num <- round(dim(table)[1]/10000)+1
num2 <- rep(10000,num)
index <- rep(c(1:num),num2)
index <- index[1:dim(table)[1]]
table <- cbind(index,table)


for (i in 1:num){
f_text <- unlist(table[table$index==i],use.names = F)

# utf-8 포멧
f_text <- enc2utf8(f_text)
# RcppMeCab 형태소 분리 토큰화 [doc_id로 매칭]
f_result <- posParallel(f_text,join=FALSE,format="data.frame")
# f_resultNN <-  f_result %>% filter(grepl("NN",f_result$pos))
write.csv(f_result,paste0(i,".csv"))
}


library(foreach)

token <- list.files(paste0(file_path,"/token"))

foreach(i = 1:length(token),combine = rbind) %do% {
tokens <- read.csv(paste0(file_path,"/token/",token[i]))
}

check <- tokens %>% group_by(token) %>% summarise(count=n()) %>% arrange(desc(count))



# 미분류 상위 몰 리스트
non_mall_list <- 
c("kolonmall.com","byseries.com","kolonsport.com","247series.co.kr","en.stylenanda.com","archive-epke.com","eqlstore.com","folderstyle.com","magjay.com",
  "maybe-baby.co.kr","miamasvin.co.kr","hanaunni.com","dabagirl.co.kr","loveme.kr","lsnmall.com","jp.66girls.com","ggsing.tw","holicholic.com","hotping.co.kr",
  "ggsing.com","gumzzi.co.kr","orr.co.kr","nabbo.co.kr","badmintonmarket.co.kr","dodry.net","flymodel.co.kr","ganaswim.cn","fromheadtotoe.kr","11am.co.kr",
  "helloyoonsoo.com","minibbong.co.kr","littleblack.co.kr","discosalon1.cafe24.com","heights-store.com","build.co.kr")
ch_list <- c("jk","cd","nb","sl","bl","sk","mt","bby","pt","sh","ct","jp","kn")
non_key <- table[table$final =="non",] %>% grepl(ch_list,table$product_name) %>% select(brand_code) %>% unique()




# #랜덤추출
# #카테고리비례추출
# f_samplen <- table[table$final_cate != "non",] %>% arrange(final_cate) %>%
#   group_by(final_cate)%>%
#   summarise(Count=n()) %>%
        #   mutate(calc=Count/dim(table)[1],
#          sample_n=round(calc*2000)) 
# 
# f_samplen <- unlist(f_samplen[,4],use.names=F)
# f_m <- strata('final_cate',size=f_samplen,method="srswor",data=table[table$final_cate != "non",] %>% arrange(final_cate))
# sample <- getdata(table[table$final_cate != "non",] %>% arrange(final_cate),f_m)
# 
# write.csv(sample,"pro_sample.csv")




# #검수
# lp <- ""
# names <- "line t shirts"
# for (i in 1:length(deplist)){
# 
#   p <- if(sum(str_locate(names,deplist[[i]])[,2],na.rm=T)==0)
#   {sum(str_locate(names,deplist[[i]])[,2],na.rm=T)}
#   else if(sum(str_locate(names,deplist[[1]])[,2],na.rm=T)-sum(str_locate(names,deplist[[i]])[,1],na.rm=T)==0)
#   {sum(str_locate(names,deplist[[i]])[,2],na.rm=T)-1}else{sum(str_locate(names,deplist[[i]])[,2],na.rm=T)}
#   lp[i] <- p
# }
# paste(names(deplist)[which(lp==max(lp) & max(lp) != 0)],collapse = ",")

# lc <- ""
# cnames <- "베스트,베스트,자켓,베스트"
# for (i in 1:length(deplist)){
#   c <- sum(ifelse(str_detect(cnames,deplist[[i]]),1,0))
#   lc[i] <- c
#  
# }
# paste(names(deplist)[which(lc==max(lc) & max(lc) != 0)],collapse = ",")

