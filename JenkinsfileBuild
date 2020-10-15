pipeline{
    agent any
    
    stages{
        stage('Build Branch'){
	    steps{
	        dir("C:\\Users\\Administrator\\AppData\\Local\\Programs\\Python\\Python38-32") {
			bat "python build.py.txt '${task}' '${trno}' '${user}' '${repo}' '${branch}'"
                } 
	        
	       }
        }
        stage('CI-Code Review'){
	    steps{
	        dir("C:\\Users\\Administrator\\AppData\\Roaming\\npm") {
                    bat 'newman run ATCCheckLegacy.json --environment Gw1Env.postman_environment.json'
             }
	    }
	}
        stage('CI-Unit Test/Coverage'){
            steps {
                 dir("C:\\Users\\Administrator\\AppData\\Roaming\\npm") {
                    bat "newman run abap_unit_coverage.json --environment Gw1Env.postman_environment.json"
            }   
            }
        }
	    
    }
}
