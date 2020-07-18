<!DOCTYPE html>
<html>
<head>
<style>
.example {
  border: 1px solid black;
  margin: 5px;
}
</style>
</head>
<body>

<div class="example">
A div with class="example"
</div>

<div class="example">
Another div with class="example"
</div>

<p class="example">This is a p element with class="example".</p>

<p>This is a <span class="example">span</span> element with class="example" inside another p element.</p>

<p>Click the button to change the background color of all elements with class="example".</p>

<button class="example" onclick="myFunction()">Try it</button>

<p><strong>Note:</strong> The querySelectorAll() method is not supported in Internet Explorer 8 and earlier versions.</p>
<style>
.foo {
    background-color: green;
    }
</style>
<script>
function myFunction() {
  var x = document.querySelectorAll(".example");
  var i;
  for (i = 0; i < x.length; i++) {
    x[i].classList.add("foo");
  }
}
</script>

</body>
</html>
