@Library('sfci-pipeline-sharedlib@master')

import net.sfdc.dci.BuildUtils
import net.sfdc.dci.MavenUtils

env.RELEASE_BRANCHES = ['main']
env.GUS_TEAM_NAME = 'UIP Quality Systems'

def envDef = [
    buildImage: 'ops0-artifactrepo1-0-prd.data.sfdc.net/uiplatform/utam:latest',
]

executePipeline(envDef) {
    stage('Init') {
        checkout scm
        mavenInit()
    }

    stage('Build') {
        mavenBuild()
    }

    if (BuildUtils.isReleaseBuild(env)) {
        stage('Prepare release') {
        }

        stage('Release') {
        }
    }
}
