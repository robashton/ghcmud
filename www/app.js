var ready = require('domready')
  , request = require('browser-request')


// throwaway garbage
ready(function() {
    var instruction = document.getElementById('instruction')
      , input = document.getElementById('input')
      , output = document.getElementById('output')
      , username = null

      input.onkeypress =  handleInput
      getInput()

      function handleInput(e) {
        if(e.keyCode === 13) {
          if(username) {
            sendCommand(input.value)
          } else {
            sendUsername(input.value)
          }
          input.value = ""
        }
      }

      function sendCommand(command) {
        request.post({ 
            uri: '/command/' + username,
            body: "command=" + command,
            encoding: "application/x-www-form-urlencoded"
          }, function(err, two, three) {
          console.log(err, two, three)
          getInput()
        })
      }

      function sendUsername(value) {
        request.get('/login/' + value, function(err) {
          addFeedback("Logged in as " + value)
          username = value
          getInput()
        })
      }

      function addFeedback(feedback) {
        var element = document.createElement("p")
        element.innerText = feedback
        output.appendChild(element)
      }

      function getInput() {
        if( !username ) {
          instruction.innerText = "What's the player name you're going as?"
        } else {
          instruction.innerText = "Gimme Move/Look/etc"
        }
      }
})

