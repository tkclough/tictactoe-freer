// This file is required by the index.html file and will
// be executed in the renderer process for that window.
// All of the Node.js APIs are available in this process.
// Backend and endpoint details.
const host     = 'http://127.0.0.1:8080'
const endpoint = '/query/'

let fetchIndices = function() {
  for (let i = 0; i < 9; i++) {
    $.getJSON(host + endpoint + i, function(data) {
      let sel = '#board #' + i 
      console.log(data)
      if (data !== null) {
        $(sel).html(data.mark)
      }
    })
  }
}

let _fetchStow = function(waitTime, maxAttempts, currentAttemptNo) {
  $.getJSON(host + stowEndpoint, function(stow) {
    console.log(stow)
    for (var i = 0; i < 4; i++) {
      for (var j = 0; j < 2; j++) {
        let sel = '#stowSpec #row' + i + ' #col' + j 
        let cont = stow.spec[i][j]
        if (cont !== null) {
          $(sel).html(cont.chemical + '<br />' + cont.amount)
        }
      }
    }
  }).fail(function() {
    if (currentAttemptNo < maxAttempts) {
      setTimeout(function() {
        _fetchStow(waitTime, maxAttempts, currentAttemptNo + 1)
      }, waitTime)
    }
  })
}

let fetchStow = function(waitTimeBetweenAttempt, maxNoOfAttempts) {
  _fetchStow(waitTimeBetweenAttempt, maxNoOfAttempts, 1)
}


//fetchStow(waitTimeBetweenAttempt, maxNoOfAttempts)

let gameStarted = false
fetchIndices()

$('#board td').click(function() {
  gameStarted = true
  let json = {index: parseInt($(this).attr('id')), mark: "O" }
  console.log(json)

  $.ajax({
    type: 'POST',
    url: host + '/set',
    data: JSON.stringify(json),
    contentType: "application/json; charset=utf-8",
    dataType: "json",
    success: function(msg) {
      console.log(msg);
    }
  })

  fetchIndices()
})