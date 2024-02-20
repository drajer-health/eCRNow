pipeline {
  agent any
  options {
    buildDiscarder(logRotator(numToKeepStr: '10'))
  }
  environment {
    AWS_CREDENTIALS = credentials('AWS-KEYS')
    AWS_ACCOUNTS = "492215550490"
    SERVICE_NAME = "medmorph"
    AWS_ENV = """${sh(
      returnStdout: true,
      script: 'env | awk -F= \'/^AWS/ {print "-e " $1}\''
    )}"""
    GIT_ENV = """${sh(
      returnStdout: true,
      script: 'env | awk -F= \'/^GIT/ {print "-e " $1}\''
    )}"""
  }
  stages {
    stage('Setup') {
      steps {
        sh '''
          docker pull $CICD_ECR_REGISTRY/cicd:latest
          docker tag $CICD_ECR_REGISTRY/cicd:latest cicd:latest
        '''
      }
    }
    stage('Build') {
      steps {
        echo 'Building Docker Image'
        sh 'docker build -t $SERVICE_NAME .'
      }
    }
    stage('Push') {
      steps {
        echo 'Pushing Docker Image'
        sh 'docker run -v /var/run/docker.sock:/var/run/docker.sock $AWS_ENV $GIT_ENV cicd push $SERVICE_NAME'
      }
    }
    stage('Environments') {
      parallel {
        stage('Consulting') {
          stages {
            stage('Deploy') {
              steps {
                echo 'Deploying to DEV'
                sh 'docker run -v /var/run/docker.sock:/var/run/docker.sock $AWS_ENV $GIT_ENV cicd deploy $SERVICE_NAME consulting $GIT_COMMIT'
              }
            }
            stage('Wait') {
              steps {
                echo 'Waiting for DEV service to reach steady state'
                sh 'docker run -v /var/run/docker.sock:/var/run/docker.sock $AWS_ENV cicd wait $SERVICE_NAME consulting'
              }
            }
            stage('Healthcheck') {
              steps {
                echo 'Checking health of DEV service'
                sh 'curl -m 10 https://medmorph.elimuinformatics.com/actuator/health'
              }
            }
          }
        }
      }
    }
  }
  post {
    unsuccessful {
      slackSend color: 'danger', channel: '#product-ops-consulting', message: "Pipeline Failed: ${env.JOB_NAME} ${env.BUILD_NUMBER} (<${env.BUILD_URL}|Open>)"
    }
    fixed {
      slackSend color: 'good', channel: '#product-ops-consulting', message: "Pipeline Ran Successfully: ${env.JOB_NAME} ${env.BUILD_NUMBER} (<${env.BUILD_URL}|Open>)"
    }
  }
}