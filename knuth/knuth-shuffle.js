//knuth-shuffle recursive
function shuffle2 (inputArray) {
  //const loopCount = 0;  
  (function loop(loopCount){
      if ( loopCount === inputArray.length ) return 
      const j = Math.floor(Math.random() * loopCount)
      const temp = inputArray[loopCount]
      inputArray[loopCount] = inputArray[j]
      inputArray[j] = temp
      loop(loopCount + 1)
  })( 0 )
  return inputArray
}