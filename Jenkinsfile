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
          sh "PUSH_CACHE=1 scripts/build.sh"
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