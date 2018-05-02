#  Spring Board   DPLYR Exercise - Refine excel 
#install.packages("tidyr")
library(dplyr)
library(tidyr)

# load the refine data into dataframe 
Refine <-  read.csv(file="C:\\Users\\Dhathri\\Desktop\\R\\refine_original.csv", header=TRUE, sep=",")

# build a list of characters to compare 
cmpr1 <-  c("p","h","i","l","i","p","s")
cmpr2 <-  c("a","k","z","o" )
cmpr3 <-  c("v","a","n", " " ,"h","o","u","t","e","n"  )
cmpr4 <-  c( "u","n","i","l","e","v","e","r")

clist1 <-  list( "philips" , cmpr1) 
clist2 <-  list( "akzo" , cmpr2) 
clist3 <-  list( "van houten" , cmpr3) 
clist4 <-  list( "unilever" , cmpr4) 

clist <- list ( clist1 ,clist2 , clist3 , clist4)


# clean up the brand names 

Get_Brand <-  function(inp_brand)
  {
  

  
  temp_count <- 0 
  temp_index <- 1 
  inp_brand_low  <- ""
  inp_brand_low <- tolower(inp_brand)

   Sim_Count <-  0 
  i <- 0
  j <- 0
  for ( i in 1:4 )
    {
       temp_str <- NA
       temp_str <-  unlist(clist[[i]][2])
       
       j <- 1 
       Sim_Count <-  0
     
       while ( j <= nchar(inp_brand_low) & j <= length(temp_str) )
         {
          
        
          if ( substr(inp_brand_low,j,j) == temp_str[j] )
          {
            Sim_Count <-   Sim_Count + 1
          }
          j <-  j + 1 
         }  #end while 
      
         if (Sim_Count > temp_count)
         {
           temp_count <- Sim_Count
           temp_index <- i
         }
       
      }   # end for 
      
      rtn_str <-  unlist(clist[[temp_index]][1])
      return(rtn_str)
  
   }  # end get brand function

# update the brand name in the refine data frame 

  for ( i in 1:nrow(Refine))
  {
    #print (paste ( "xxx" , as.character(Refine[i,1]) ))
    Refine[i,1] <-   Get_Brand(as.character(Refine[i,1])  )
     #print (paste ( "yyy" , as.character(Refine[i,1]) ))
  }


Refine

# split  the product code number column into product code and number 
names(Refine)

Refine <- separate(Refine,Product.code...number,into = c("product_code" , "product_number"), sep = '-' )
Refine


#  Add a new column for product category which describes the product code 

ret_product_desc <- function(code)
{
  if (code == "p")
  {
    return ("Smartphone")
  }
  else if (code == "v")
  {
    return ("TV")
  }
  else if (code == "x")
  {
    return ("Laptop")
  }
  else if (code == "q")
  {
    return ("Tablet")
  }
  
}


Refine$product_category <- sapply(Refine$product_code,ret_product_desc)

Refine

# combine address, city, country into full address 

Refine <-  unite(Refine,"full_address", c("address","city","country"), sep = ",")
Refine

