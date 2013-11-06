angular.module('codefaceApp', ['$strap.directives']).
    config(['$routeProvider', function($routeProvider) {
        $routeProvider
	    .when('/home', { templateUrl: 'home.html' })
            .when('/sshots', { templateUrl: 'sshots.html' })
            .when('/examples/analysis', { templateUrl: 'examples.html' })
            .when('/examples/linux', { templateUrl: 'linux.html' })
            .when('/stat_details', { templateUrl: 'stat_details.html' })
            .when('/code', { templateUrl: 'code.html' })
            .otherwise({redirectTo: '/home'});
    }], ['$locationProvider', function($locationProvider){
        //$locationProvider.html5Mode(true);
    }]);     
