#!groovy

pipeline {
  agent {
    dockerfile true
  }
  environment {
    NEXUS = credentials('exchange-nexus')
    GITHUB_ORG = 'aml-org'
    GITHUB_REPO = 'amf-core'
  }
  stages {
    stage('Test') {
      steps {
        wrap([$class: 'AnsiColorBuildWrapper', 'colorMapName': 'XTerm']) {
          sh 'sbt -mem 4096 -Dfile.encoding=UTF-8 clean coverage test coverageReport'
        }
      }
    }
    stage('Coverage') {
      when {
        branch 'master'
      }
      steps {
        wrap([$class: 'AnsiColorBuildWrapper', 'colorMapName': 'XTerm']) {
          withCredentials([[$class: 'UsernamePasswordMultiBinding', credentialsId: 'sonarqube-official', passwordVariable: 'SONAR_SERVER_TOKEN', usernameVariable: 'SONAR_SERVER_URL']]) {
            sh 'sbt -Dsonar.host.url=${SONAR_SERVER_URL} sonarScan'
          }
        }
      }
    }
    stage('Publish') {
      when {
        branch 'master'
      }
      steps {
        wrap([$class: 'AnsiColorBuildWrapper', 'colorMapName': 'XTerm']) {
          sh '''
              echo "about to publish in sbt"
              sbt publish
              echo "sbt publishing successful"
          '''
        }
      }
    }
    stage('Tag version') {
      when {
        branch 'master'
      }
      steps {
        withCredentials([[$class: 'UsernamePasswordMultiBinding', credentialsId: 'github-salt', passwordVariable: 'GITHUB_PASS', usernameVariable: 'GITHUB_USER']]) {
          sh '''#!/bin/bash
                echo "about to tag the commit with the new version:"
                version=$(sbt version | tail -n 1 | grep -o '[0-9].[0-9].[0-9].*')
                url="https://${GITHUB_USER}:${GITHUB_PASS}@github.com/${GITHUB_ORG}/${GITHUB_REPO}"
                git tag $version
                git push $url $version
                echo "tagging successful"
          '''
        }
      }
    }
  }
}
