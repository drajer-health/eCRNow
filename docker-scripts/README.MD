Prerequisites
 
1) Docker to be installed.
	Follow the Docker Installation Instructions "https://docs.docker.com/get-docker/"

2) eCRNow Application should be built. 
	Follow the instructions in README.md for getting the source code and building the application. 

Below are options to build and run docker images

*** Skip to Running Docker Instance, if downloading the images and not building docker images locally
*** Run the scripts from the same checkout folder 

Build Docker Instances:

1) Build docker image locally with Postgres database and eCRNow application

	a) Run below command to generate docker image ecrnowapp
            
            docker build -t ecrnowapp -f docker-scripts/Dockerfile-All .
            docker tag ecrnowapp ecrnow/ecrnowapp:1.0

2) Build docker image locally for eCRNow application

	a) Edit Dockerfile-App to update the postgres database information

			ENV app_db=localhost
			ENV app_db_port=5432
			ENV app_db_name=postgres
			ENV app_db_user=postgres
			ENV app_db_pwd=postgres

	b) Run below command to generate docker image ecrnow-app
            
            docker build -t ecrnow-app -f docker-scripts/Dockerfile-App .
            docker tag ecrnow-app ecrnow/ecrnow-app:1.0

3) Build docker images for postgres and eCRNow application separately

	a) Refer step 2 to build eCRNow application
	
	b) Run below command to generate docker image ecrnow-db for postgres database
		
			docker build -t ecrnow-db -f docker-scripts/Dockerfile-Db .
			docker tag ecrnow-db ecrnow/ecrnow-db:1.0

Running Docker Instances:

1) Running ecrnowapp docker image which includes database and eCRNow application
   
   	  docker run -d --name ecrnowapp -v <<folder location to be shared with docker as mount>>:/mnt -p <<port>>:8081 ecrnowapp 
      
      Eg:
      
      docker run -d --name ecrnowapp -v /home/eCRNow/eCRNow:/mnt -p 8081:8081 ecrnowapp      
      
2) Running eCRNow database and application
 
** NOTE: docker-compose to be installed, refer https://docs.docker.com/compose/install/       

	a) Edit docker-compose.yml as necessary to update volume 
      
      docker-compose -f docker-scripts/docker-compose.yml up -d
      
3) Running eCRNow application using in-house or external Postgres database

	 a) Set the below environment variables to reflect external Postgres database
			export app_db=localhost
			export app_db_port=5432
			export app_db_name=postgres
			export app_db_user=postgres
			export app_db_pwd=postgres

            docker run -d --name ecrnow-app -v <<folder location to be shared with docker as mount>>:/mnt -p <<port>>:8081 ecrnow-app 
      
	        Eg:
	      
	        docker run -d --name ecrnow-app -v /home/eCRNow/eCRNow:/mnt -p 8081:8081 ecrnow-app
      
4) Running eCRNow application and postgres database as standalone docker containers (optional)
      
      a) eCRNow database:
      		
      		docker run -d --name ecrnow-db -v <<folder location to be shared with docker as mount>>:/mnt -p 5432:5432 ecrnow-db
      	
      		Eg:
      		docker run -d --name ecrnow-db -v /home/eCRNow/eCRNow:/mnt -p 5432:5432 ecrnow-db


	  b) Set the below environment variables to reflect external Postgres database
			export app_db=localhost
			export app_db_port=5432
			export app_db_name=postgres
			export app_db_user=postgres
			export app_db_pwd=postgres
			            
      c) eCRNow application: 
            
	        docker run -d --name ecrnow-app -v /home/eCRNow/eCRNow:/mnt -p 8081:8081 ecrnow-app
            
Cleaning Docker Instances:

a) Stop any docker instance
 		   
 		  docker stop ecrnowapp
 		  docker stop ecrnow-app
 		  docker stop ecrnow-db
 		  
b) Remove any stopped instance
          
          docker rm ecrnowapp
          docker rm ecrnow-app
          docker rm ecrnow-db
          
c) Stop and remove docker-compose 
  
          docker-compose -f docker-scripts/docker-compose.yml down
          
Remove all images and clean up 

	docker system prune --all

 