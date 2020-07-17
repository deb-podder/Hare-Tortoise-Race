#================================================== QUESTION =======================================================

# A hare & tortoise went for a race of 200 kilometers. The hare is fast wants to win at any condition. He starts the 
# journey at 12 km per hour. Also due to the extra energy he expends, he ends up reducing the speed by 2 km every 
# hour. When the speed is at the verge of becoming 0, the hare sleeps for anything between 2 and 5 hours. When he 
# wakes up again, regains the speed at 12 km per hour again and keeps losing at 2 km every hour like before before 
# he goes for sleep. The cycle continues.The tortoise starts the race at 2km per hour and steadily increases the 
# pace by 5% per hour. Simulate this entire logic using a R program playing the game for 100 times. In those 100 
# games, who wins most of the race?

#-------------------------------------------------------------------------------------------------------------------

#==================================================== SOLUTION =====================================================


###################################################### LOGIC #######################################################

# - First solution of a single race for both Hare and Tortoise need to be build.
# - Total distance to be covered by both is 200km.
# - Need to calculate the time required by Hare and Tortoise individually to cover 200km.
# - For every iteration one hour will be added for both Hare and Tortoise.
# - For Hare, the speed will start from 12km/hr and will get decreased by 2km/hr every hour and when speed will 
#   become 0 after every iteration, the Hare will take rest.
# - In case of Hare ,the time of rest also need to be considered, randomly something between 2 and 5 hour after 
#   each cycle and to be added with the total time taken by Hare to cover 200km.
# - For Tortoise, the initial speed will be 2km/hr and will increase by 5% of the last attained speed every hour.
# - Out of them, one who will take less time to reach 200km mark will win.
# - Need to repeat the same race simulation 100 times and need to check out of these 100 times who wins the race 
#   most times.

################################################## DATA CONTAINERS #################################################

# - A container to store the total distance covered by Hare.
# - A container to store the total distance covered by Tortoise.
# - A container to store the total time taken by Hare to reach 200km
# - A container to store the total time taken by Tortoise to reach 200km
# - A container to store the cumulative distance covered by Hare after every iteration
# - A container to store the cumulative distance covered by Tortoise after every iteration
# - A container to store the speed of Tortoise
# - A container to generate the random amount of hour of rest

################################################## PROCEDURES ######################################################

# - ruinf(1,2,5) - To generate a random value between 2 and 5 from a uniform distribution.
# - replicate(100, race()) - To replicate a user defined function race() to simulate the process 100 times.
# - which() - To get the index places where Hare won and where Tortoise won out of the 100 races simulated.


############################# User defined function to simulate a single race ######################################

race = function(){
  
  dh = 0
  dt = 0
  th = 0
  tt = 0
  dist_h = c()
  dist_t = c()
  speed = 2

  
############################################## HARE SIMULATION #####################################################
  
  
  #To check the total distance covered shouldnot exceed 200km
  while(dh<=200){
    
    d=12 #The initial distance for every cycle is 12km
    
    for(i in 1:6){ #For every cycle before the speed becomes 0km/hr
      dh = dh+d
      
      if(dh<=200){ #To check whether the total distance covered doesnot exceed 200km
        
        d = d-2
        th = th+1 #Time gets added by 1hr for every iteration
        dist_h = c(dist_h,dh) #The total distance covered at the end of every iteration
      
        }else{
          
        break #The loop will break if the total distance covered exceeds 200km
      }
    }
    
    rest =  round(runif(1,2,5),2) #Any random value between 2hr to 5hr for rest
    th = th+rest #Rest time will get added to the total time
  }
 
  
  if(dist_h[length(dist_h)]<200){
  #Checking the last distance updated in the vector for Hare which will be 198km
    
    extra_h = round((200-dist_h[length(dist_h)])/d,2) #Time taken to cover the extra 2Km
    total_time_h = th+extra_h #Total time to cover exact 200km
  
  }else{
    
    total_time_h = th #If last total distance covered is exactly 200km
  }
  
  
############################################ TORTOISE SIMULATION ###################################################
  
  
  #To check the total distance covered shouldnot exceed 200km 
  while(dt<=200){
    
    dt = round((dt+speed),2) #The initial distance covered will be 2km and speed will be 2km/hr
    
    if(dt<200){ #To check whether the total distance covered doesnot exceed 200km
      
      speed=round(1.05*speed,3) #Speed gets increased by 5%
      tt = tt+1 #Time gets added by 1hr for every iteration
      dist_t = c(dist_t,dt) #The total distance covered at the end of every iteration
    
    }else{
      
      break #The loop will break if the total distance covered exceeds 200km
    }
  }
 
  if(dist_t[length(dist_t)]<200){
  #Checking the last distance updated in the vector for Tortoise which will be 191.673km approx
    
    extra_t = round((200-dist_t[length(dist_t)])/speed,2) #Time taken to cover the extra distance
    total_time_t = tt+extra_t #Total time to cover exact 200km
  
  }else{
  
      total_time_t = tt #If last total distance covered is exactly 200km
  }

  
############################################## To check who won ####################################################
  
  
  if(total_time_h<total_time_t){ #To check if Hare won by taking less time
    
    return("Hare")
  
  }else if(total_time_h==total_time_t){ #To check if both took same time
    
    return("Draw")
  
  }else{
    
    return("Tortoise") #To check if Tortoise won by taking less time
  }
  
}


####################################### To simulate the race 100 times #############################################


race_simulation = c(replicate(100, race())) #The udf will be called 100times to do 100 simulation
print(race_simulation) 


############################# To check who won maximum times out of 100 simulation #################################


if(length(which("Hare" == race_simulation))>length(which("Tortoise" == race_simulation))){
  
  print(paste("Hare won most of the race :",length(which("Hare" == race_simulation)),"times"))
  
}else if(length(which("Hare" == race_simulation))==length(which("Tortoise" == race_simulation))){
  
  print(paste("Both Tortoise and Hare won for the same number of times",
              length(which("Hare" == race_simulation))))
  
}else{
  
  print(paste("Tortoise won most of the race :",
              length(which("Tortoise" == race_simulation)),"times"))
  
}

################################################## END #############################################################