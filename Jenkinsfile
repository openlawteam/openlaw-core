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
    stage('Cached Build') {
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

    stage('Test') {
      failFast true
      parallel {

        stage('Run Tests') {
          steps {
            container('jx-base') {
              sh "docker run --network=host --rm openlaw/core ./scripts/test.sh"
            }
          }
        }

        stage('Coverage Report') {
          steps {
            container('jx-base') {
              sh "docker run --network=host --rm openlaw/core ./scripts/coverage_report.sh"
            }
          }
        }

      }
    }

    stage('Release') {
      // TODO: we should verify master branch as well?
      // TODO: how to configure to ensure JenkinsX gets github "release" event notifications?
      when { tag "v*" }
      steps {
        container('jx-base') {
          sh "docker run --network=host --rm openlaw/core ./scripts/release.sh"
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