//set CORS to call "tvscore" package on public server
ocpu.seturl("//172.17.0.61/ocpu/library/mlpredict/R")


//call R function: tvscore::tv(input=data)
$("#submitbutton").click(function(){
  console.log($("#teamA option:selected").text());
  console.log($("#teamB option:selected").text());

  var mydata = [
    {teamA : $("#teamA option:selected").text(), 
     teamB : $("#teamB option:selected").text() }
  ];
  
  var req = ocpu.rpc("mlpredict",{
    input : mydata
  }, function(output){
      console.log(output);
    $("tbody").empty();
    $.each(output, function(index, value){
      var html = "<tr><td>" + value.teamA + "</td><td>" + value.teamB + "</td><td>" + value.predictedWinner + "</td></tr>";
      $("tbody").append(html);
    });
  });

  //optional
  req.fail(function(){
    console.error("R returned an error: " + req.responseText); 
  });

});
