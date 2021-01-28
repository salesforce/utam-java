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
        if(BuildUtils.isReleaseBuild(env)) {
            print('Skipping the build as it will be run during the release prepare')
        } else {
            mavenBuild()
        }
    }

    if (BuildUtils.isReleaseBuild(env)) {
        stage('Prepare release') {
            mavenVersionsSet([managed: true])
            mavenStageArtifacts()
        }

        stage('Release') {
            mavenPromoteArtifacts()
        }
    }
}
