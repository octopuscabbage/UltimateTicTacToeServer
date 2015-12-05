var app = angular.module('myApp', []);
app.controller('myCtrl', function($scope,$http) {
	$scope.board = "test"
	$scope.findGame = function(){
			$http.get("http://localhost:8080/game/" + $scope.name1 + "/" + $scope.name2 + "/view").then(function(response) {$scope.data = response.data;})
	}
	$scope.formatBoard = function(){
		var output = ""
		for(i = 0; i < 9; i++){
			for(j = 0; j < 9; j++){
				if($scope.data.board[i][j] == "Empty"){
					output += "E"
				}
				else{
					output += $scope.data.board[i][j]
				}
				output += "|"
			}
			output += " <br> "
	}
	return output
      }
});

