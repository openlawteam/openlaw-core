pipeline {
  agent {
    label "jenkins-jx-base"
  }
  environment {
    ORG = 'openlawteam'
    APP_NAME = 'openlaw-core'
    CHARTMUSEUM_CREDS = credentials('jenkins-x-chartmuseum')
  }
  stages {
    stage('CI Build and push snapshot') {
      when {
        branch 'PR-*'
      }
      steps {
        container('jx-base') {
          sh "PUSH_CACHE=1 \
              REMOTE_IMAGE='889468772737.dkr.ecr.us-west-2.amazonaws.com/openlawteam/openlaw-core' \
              BUILD_PARAMS='--network=host' \
              scripts/build.sh"
        }
      }
    }
  }
  post {
        always {
          cleanWs()
        }
  }
}