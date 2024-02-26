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

file_path <- "your_path"
setwd(file_path)

# 패션 상품 리스트
fa <- fread(file.path(file_path,"fashion_categories.csv"),encoding = "UTF-8")
# 패션 고객사 정보
mallid <- fread(file.path(file_path,"fashion_mall_list.csv"),encoding = "UTF-8")
#고객사 필터
mlist <- unique(mallid$brand_code)

# 상품 정보
pro <- fread(file.path(file_path,"products.csv"),encoding = "UTF-8")
pro <- pro %>% filter(brand_code %in% mlist)
# 카테고리 정보
pro_cate <- fread(file.path(file_path,"product_categories.csv"),encoding = "UTF-8")
pro_cate <- pro_cate %>% filter(brand_code %in% mlist)
pro_cate2 <- fread(file.path(file_path,"brand_product_categorizations.csv"),encoding = "UTF-8")
pro_cate2 <- pro_cate2 %>% filter(brand_code %in% mlist)



#카테고리 사전
falist <- as.list(fa$category)


# 카테고리 이름 테이블
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


getwd()
fwrite(pro_test,"test.csv")
gc()
options(future.fork.multithreading.enable = FALSE)

