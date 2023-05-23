let routes () =
  Dream.run @@ Dream.logger @@ Dream.memory_sessions
  @@ Dream.router
       [
         Dream.get "/" View.Home.home_controller;
         Dream.get "/:id" View.Home.single_controller;
         Dream.post "/create" View.Home.create_controller;
       ]
