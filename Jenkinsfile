pipeline {
  agent  {
    label "jenkins-jx-base"
  }
  environment {
    ORG = 'openlawteam'
    APP_NAME = 'openlaw-core'
    CHARTMUSEUM_CREDS = credentials('jenkins-x-chartmuseum')
  }
  stages {
    stage('CI Cached Build') {
      // when {
      //   branch 'PR-*'
      // }
      steps {
        container('jx-base') {
          sh "BRANCH=$GIT_BRANCH \
              PUSH_CACHE=1 \
              REMOTE_IMAGE='889468772737.dkr.ecr.us-west-2.amazonaws.com/openlawteam/openlaw-core' \
              BUILD_PARAMS='--network=host' \
              ci/build.sh"
        }
      }
    }

    stage('CI Test') {
      steps {
        container('jx-base') {
          sh "docker run --rm -v ${PWD}/scripts:/scripts openlaw/core /scripts/tesh.sh"
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