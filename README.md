# Hare-Tortoise-Race
## QUESTION
A hare & a tortoise went for a race of 200 kilometers. The hare is fast wants to win at any condition. He starts the journey at 12 km per hour. Also due to the extra energy he expends, he ends up reducing the speed by 2 km every hour. When the speed is at the verge of becoming 0, the hare sleeps for anything between 2 and 5 hours. When he wakes up again, regains the speed at 12 km per hour again and keeps losing at 2 km every hour like before before he goes for sleep. The cycle continues.The tortoise starts the race at 2km per hour and steadily increases the pace by 5% per hour. Simulate this entire logic using a R program playing the game for 100 times. In those 100 games, who wins most of the race?  
## SOLUTION 

#### LOGIC
- First solution of a single race for both Hare and Tortoise need to be build. 
- Total distance to be covered by both is 200km. 
- Need to calculate the time required by Hare and Tortoise individually to cover 200km. 
- For every iteration one hour will be added for both Hare and Tortoise. 
- For Hare, the speed will start from 12km/hr and will get decreased by 2km/hr every hour and when speed will become 0 after every iteration, the Hare will take rest. 
- In case of Hare ,the time of rest also need to be considered, randomly something between 2 and 5 hour after each cycle and to be added with the total time taken by Hare to cover 200km. 
- For Tortoise, the initial speed will be 2km/hr and will increase by 5% of the last attained speed every hour. 
- Out of them, one who will take less time to reach 200km mark will win. 
- Need to repeat the same race simulation 100 times and need to check out of these 100 times who wins the race most times. 

#### DATA CONTAINERS
- A container to store the total distance covered by Hare
- A container to store the total distance covered by Tortoise.
- A container to store the total time taken by Hare to reach 200km
- A container to store the total time taken by Tortoise to reach 200km 
- A container to store the cumulative distance covered by Hare after every iteration 
- A container to store the cumulative distance covered by Tortoise after every iteration 
- A container to store the speed of Tortoise 
- A container to generate the random amount of hour of rest

#### PROCEDURES
- ruinf(1,2,5) - To generate a random value between 2 and 5 from a uniform distribution.
- replicate(100, race()) - To replicate a user defined function race() to simulate the process 100 times.
- which() - To get the index places where Hare won and where Tortoise won out of the 100 races simulated.
