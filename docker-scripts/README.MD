Prerequisites

1) Docker to be installed.
	Follow the Docker Installation Instructions "https://docs.docker.com/get-docker/"

2) eCRNow Application should be built.
	Follow the instructions in README.md for getting the source code and building the application.

Steps:

1) Create a network to connect application and database.

			docker network create ecrnow-net

2) Build Postgres database as below and run the docker container.

			# Remove the folder if exists
			rm -rf pgsql

			# Build the ecrnow-db as:
			docker build -t ecrnow-db -f docker-scripts/Dockerfile-Db .

			# Run the ecrnow-db as:
			docker run -d --name ecrnow-db --net=ecrnow-net -v <<Path to store postgres data files >>:/mnt -p 5432:5432 ecrnow-db

			Eg:
			docker run -d --name ecrnow-db --net=ecrnow-net -v /opt/app/ecrnow:/mnt -p 5432:5432 ecrnow-db

3) Build the ecrnow-app, make sure ecr-now.war is built

			# Edit the application-docker.properties and ensure it matches with application.properties, review and make changes as necessary
				 ** Make sure all the supporting documents and folder are created / updated as needed in application-docker.properties.

				 ersd.file.location=${app_mnt}/documents/app-artifacts/ersd.json
				 schematron.file.location=${app_mnt}/documents/app-artifacts/CDAR2_IG_PHCASERPT_R2_STU1.1_SCHEMATRON.sch
				 xsd.schemas.location=${app_mnt}/documents/app-artifacts/CDA_SDTC.xsd
				 kar.directory=${app_mnt}/documents/app-artifacts/kars
				 bsa.output.directory=${app_mnt}/documents/app-artifacts/bsa-output//
				 custom-query.directory=${app_mnt}/documents/app-artifacts/custom-queries

			# Make sure ecrnow.war is built

				 mvn clean install -Dskip.unit.tests=true -Dskip.integration.tests=true

			# Edit Dockerfile-App to update the postgres database information

					# Mount point for the logs and resource files.
					ENV app_mnt=/mnt

					# ecrnow-db is the database docker container name
					ENV app_db=ecrnow-db

					ENV app_db_port=5432
					ENV app_db_name=postgres
					ENV app_db_user=postgres
					ENV app_db_pwd=postgres

			# Build the ecrnow-app as:
			docker build -t ecrnow-app -f docker-scripts/Dockerfile-App .

			# Run the ecrnow-app as:
			docker run -d --name ecrnow-app --net=ecrnow-net -v <<Path where logs and documents are shared to ecrnow application>> :/mnt -p 8081:8081 ecrnow-app      

			Eg:
			docker run -d --name ecrnow-app --net=ecrnow-net -v /opt/app/ecrnow:/mnt -p 8081:8081 ecrnow-app      


4) Cleaning Docker Instances:

			a) Stop any docker instance

			 		  docker stop ecrnow-app
			 		  docker stop ecrnow-db

			b) Remove any stopped instance

			          docker rm ecrnow-app
			          docker rm ecrnow-db
								docker network rm ecrnow-net

			Remove all images and clean up

				docker system prune --all
