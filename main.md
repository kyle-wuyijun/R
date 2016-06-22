
#--------------------------------------------------------设置厕所的数量number_WC


#设置厕所的队列line_WC
#设置厕所时间队列time_line_WC
#设置等待队列line_waiting
#设置等待时间队列time_line_waiting
#设置每次上小便的时间WC1_time
#设置每次上大便短时间WC2_time
#----------------感觉缺乏面向对象的思维----------

HowmanyWC<-function(alldetail,number_room,time_end,number_WC,logfile=0){
time_start=0
#-------------------------------------------------------------------------------------设置房间的数量number_room
#-------------------------------------------------------------------------------------设置各个房间的人的资料矩阵alldetail

#---------------模块1：生成所有小组的特征量，包括组别、距离、小便时间均值、大便时间均值，保存在list格式中--------
matrix_room<-function(alldetail,number_room){
  if(length(alldetail)!=number_room*4){
    cat("请检查你的房间数量，跟数据集中体现的房间数量不一致。\n")
    cat("房间数量是：")
    cat(number_room,"\n")
    cat("数据集中的数量是：")
    cat(length(alldetail)/4,"\n")
  }
  else{
    detail=matrix(alldetail,nrow=number_room,ncol=4,byrow=TRUE)
    all_class_list=c()
    for(i in 1:number_room){
      number_of_people=detail[i,1]
      dstns=detail[i,2]
      WC1=detail[i,3]
      WC2=detail[i,4]
      
      label_of_class=rep(i,number_of_people)
      
      label_of_distance=rep(dstns,number_of_people)
      
      WCtime1_list=round(rnorm(number_of_people,WC1,30))
      WCtime1_list[WCtime1_list<=0]=0
      WCtime1_list[WCtime1_list>=WC1+60]=WC1+60 #设置小便的时间，以120分钟为均值，20为标准差的正态分布，超过3倍标准差的，按照3倍计算
      
      WCtime2_list=round(rnorm(number_of_people,WC2,30))
      WCtime2_list[WCtime2_list<=0]=0
      WCtime2_list[WCtime2_list>=WC2+60]=WC2+60 #设置小便的时间，以120分钟为均值，20为标准差的正态分布，超过3倍标准差的，按照3倍计算
      
      
      classx=cbind(label_of_class,label_of_distance,WCtime1_list,WCtime2_list)
      classx=data.frame(classx)
      colnames(classx)=c("class","distance","freq_WC1","freq_WC2")
      classx=list(classx)
      #classx=list(array(c(label_of_class,label_of_distance,WCtime1_list,WCtime2_list),c(number_of_people,4),dimnames=list("class","distance","WCtime1","WCtime2")))
      all_class_list=c(all_class_list,classx)
      
    }
    return(all_class_list)
  }
}
#alldetail=c(20,20,120,400,10,20,120,400)
#a<-matrix_room(alldetail,2)
#设置人数p_number_people
#设置人距离厕所距离p_distance
#设置人上WC小便的时间间隔均值freq_WC1
#设置人上WC大便的时间间隔均值freq_WC2

#上述4个数值组成alldetal中的一行，每一个办公室为一行


#计算队列初始化
temporary_line1=data.frame(0,0,0,0)
temporary_line2=data.frame(0,0,0,0)
waiting_line=data.frame(0,0,0,0)
WC_line=data.frame(0,0,0,0)
colnames(temporary_line1)=c("class","distance","freq_WC1","freq_WC2")
colnames(temporary_line2)=c("class","distance","freq_WC1","freq_WC2")
colnames(waiting_line)=c("class","distance","freq_WC1","freq_WC2")
colnames(WC_line)=c("class","distance","freq_WC1","freq_WC2")
temporary_line1=temporary_line1[-1,]
temporary_line2=temporary_line1[-1,]
waiting_line=waiting_line[-1,]
WC_line=WC_line[-1,]

#将各个组的详情输入矩阵
detailmatrix=matrix(alldetail,nrow=number_room,ncol=4,byrow=TRUE)

#生成办公室队列
classdetail_matrix=matrix_room(alldetail,number_room)

#############################等候时间记录+##################
waiting_time=0
waiting_cishu=0
###############################################


#5个队列，座位list（内含多个队列），去队列，等候队列，WC队列，回队列
while(time_start<=time_end)
{

  #5个队列，一个“去队列”，一个“回队列”，一个“排队队列”，一个“厕所队列”。回合开始的时候，全部队列现有元素的时间，-1
  #在路上：临时队列中的，每个人距离厕所的距离减1，不为0的WC1或者WC2减1
  #1
  if (nrow(temporary_line1)!=0)
  { temporary_line1[,2:4]=temporary_line1[,2:4]-1
  temporary_line1[temporary_line1[,2]<0,2]=0
  temporary_line1[temporary_line1[,3]<0,3]=0
  temporary_line1[temporary_line1[,4]<0,4]=0
  }
  #2
  if (nrow(temporary_line2)!=0)
  { temporary_line2[,2:4]=temporary_line2[,2:4]-1
  temporary_line2[temporary_line2[,2]<0,2]=0
  temporary_line2[temporary_line2[,3]<0,3]=0
  temporary_line2[temporary_line2[,4]<0,4]=0
  }
  #3
  #在厕所内：
  if (nrow(WC_line)!=0)
  { WC_line[,3:4]=WC_line[,3:4]-1
  WC_line[WC_line[,3]<0,3]=0
  WC_line[WC_line[,4]<0,4]=0
  }
  #4
  #在厕所门外
  if (nrow(waiting_line)!=0)
  { waiting_line[,3:4]=waiting_line[,3:4]-1
  waiting_line[waiting_line[,3]<0,3]=0
  waiting_line[waiting_line[,4]<0,4]=0
  waiting_time=waiting_time+nrow(waiting_line)
  }
  #5
  #在办公室：对坐在办公室的人的WC1和WC2减1，如果等于0，则进入“去队列”
  for (i in 1:number_room)
  {
  if (nrow(classdetail_matrix[[i]])!=0)
  {
  index1=rep(0,nrow(classdetail_matrix[[i]]))
  classdetail_matrix[[i]][,3:4]=classdetail_matrix[[i]][,3:4]-1
  
  classdetail_matrix[[i]][classdetail_matrix[[i]][,3]<0,3]=0
  classdetail_matrix[[i]][classdetail_matrix[[i]][,4]<0,4]=0
  for (m in 1:length(classdetail_matrix[[i]][,3]))
  {
    if (classdetail_matrix[[i]][m,3]==0 | classdetail_matrix[[i]][m,4]==0)
    {temporary_line1=rbind(temporary_line1,classdetail_matrix[[i]][m,])
    index1[m]=1
    }
    
    
  }
  classdetail_matrix[[i]]=classdetail_matrix[[i]][index1==0,]
  }
  }


  #厕所内出厕所的算法  问题：厕所内需要考虑小便时间和大便时间，小便时间为1，大便时间为10，
  #需要在排队队列，和来队列中，赋值
  if(nrow(WC_line)!=0){
  index1=rep(0,nrow(WC_line))
  for (i in 1:length(WC_line[,1]))
  {
    if(WC_line[i,3]==0&WC_line[i,4]>=5|WC_line[i,4]==0&WC_line[i,3]>=5|WC_line[i,3]==0&WC_line[i,4]==0)
    {  #如果小便时间为0，大便时间在5min内，则不出去，等大便完再出去。
      #要出去的人，如果是小便时间结束了，则从detailmatrix中，将所属组的小便时间均值赋予。大便同理。
      WC_line[i,2]=detailmatrix[WC_line[i,1],2]
      if (WC_line[i,3]==0)
      {WC_line[i,3]=rnorm(1,detailmatrix[WC_line[i,1],3],30)
       if (WC_line[i,3]<10)WC_line[i,3]=10
      }
      if (WC_line[i,4]==0)
      {WC_line[i,4]=rnorm(1,detailmatrix[WC_line[i,1],4],30)
       if (WC_line[i,4]<60)WC_line[i,4]=60
      } 

      index1[i]=1
      temporary_line2=rbind(temporary_line2,WC_line[i,])
    }
    
  }
  WC_line=WC_line[index1==0,]
  }
  #排队列中进入厕所的算法
  
  
  #等候队列的人，去WC队列 #waiting_line
  if (nrow(waiting_line)!=0){
  index1=rep(0,nrow(waiting_line))
  for (i in 1:nrow(waiting_line))
  {
    if(waiting_line[i,3]==0|waiting_line[i,4]==0)
    { 
      if(nrow(WC_line)<number_WC)
      { if(waiting_line[i,3]==0){waiting_line[i,3]=1}#小便时间
        if(waiting_line[i,4]==0){waiting_line[i,4]=10}#大便时间
        WC_line=rbind(WC_line,waiting_line[i,])
        index1[i]=1
      }
    }
    
    
  }
  waiting_line=waiting_line[index1==0,]
  
  }
  
  
  
  #去队列的人，判断等候队列人数是否为0，是，则判断WC队列的空位，进入WC队列多余的进入等候队列。如果等候队列不为0，则进入等候队列 
if(nrow(temporary_line1)!=0)
{
  index1=rep(0,nrow(temporary_line1))
  for (i in 1:nrow(temporary_line1))
  {
   if(temporary_line1[i,2]==0)
     {   if(temporary_line1[i,3]==0|temporary_line1[i,4]==0)
             { if(nrow(WC_line)<number_WC)
               {if(temporary_line1[i,3]==0){temporary_line1[i,3]=2}
                if(temporary_line1[i,4]==0){temporary_line1[i,3]=10}
                WC_line=rbind(WC_line,temporary_line1[i,])
                index1[i]=1
               }
               else
               {waiting_line=rbind(waiting_line,temporary_line1[i,])
                index1[i]=1
                waiting_cishu=waiting_cishu+1
               }
             }
     }
   }  
  temporary_line1=temporary_line1[index1==0,]
}
 
  
  #回队列返回座位队列
if (nrow(temporary_line2)!=0)
{
  index1=rep(0,nrow(temporary_line2))
  for (i in 1:nrow(temporary_line2))
  {
    if(temporary_line2[i,2]==0)
    { 
      temporary_line2[i,2]=detailmatrix[temporary_line2[i,1],2]
      classdetail_matrix[[temporary_line2[i,1]]]=rbind(classdetail_matrix[[temporary_line2[i,1]]],temporary_line2[i,])
      index1[i]=1
    }
  }
  temporary_line2=temporary_line2[index1==0,]
}
  time_start=time_start+1

  
if (logfile==1)
  {
    if(time_start<5)
       {
        print(time_start)
        
        
        print("t1")
        print(temporary_line1)
        
        
        print("waiting_line")
        print(waiting_line)
        
        print("WC_line")
        print(WC_line)
        
        print("t2")
        print(temporary_line2)
        
        print("waiting time")
        print(waiting_time)
        print("waiting cichu")
        print(waiting_cishu)
        print("___________________________________________________________________________________________")  
        }  
  }  
  
}


number_allpeople=sum(detailmatrix[,1])
return(c(waiting_time/waiting_cishu,waiting_cishu))

}





#some examples #1
#I will build the dataset include 2 rooms.
#one contains 100people,each of them spending 1min to go to the WC,and every 120mins they'll pee and every 360mins they'll shit
#another contains 100people too,and take 2min to the WC,every 120mins they'll pee and every 360mins they'll shit

alldetail=c(100,1,120,360,100,2,120,360)
HowmanyWC(alldetail,2,600,4)






cishu=c()
times=c()
for (m in c(25,50,75,100,125,150)){
   alldetail=c(m,1,120,360)
   for (i in 1:10)
    { a=HowmanyWC(alldetail,1,600,i)
            if (a[1]!="NaN")
                {
                  times=c(times,a[1])
                }
            else
              {
                times=(c(times,0))
              }
     cishu=c(cishu,a[2])
    }
}

array(cishu,dim=10)
