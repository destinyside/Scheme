 ;server.scm
(declare (uses tcp))
(define l (tcp-listen 4242))
(define-values (i o) (tcp-accept l))

(write-line "<!--img src=\"../static/0014.jpg\" style=\"max-width:100%;\" /--!>
<!DOCTYPE html>  
<html>  
<head>  
<meta charset=\"UTF-8\"/>  <title>网址导航</title>  <head> <center><body style=\"background:url('../static/0015.png');background-repeat:no-repeat;background-position:center;\"> 
<canvas id=\"canvas\" width=\"200\" height=\"200\">你的浏览器不支持canvas元素，请更换更先进的浏览器。</canvas>  
<script>  
var canvas = document.getElementById('canvas');  
if (canvas.getContext) {  
	var ctx = canvas.getContext('2d');  
	ctx.lineWidth = 8;  
	ctx.shadowOffsetX = 3;  
	ctx.shadowOffsetY = 3;  
	ctx.shadowBlur = 2;  
	ctx.font = '16px monospace';  
	var startAngle = -Math.PI / 2;  

	function drawClock() {  
		var time = new Date();  
		var hours = time.getHours();  
		var am = true;  
		if (hours >= 12) {  
			hours -= 12;  
			am = false;  
		}  
		var minutes = time.getMinutes();  
		var seconds = time.getSeconds();  

		ctx.clearRect(0, 0, 200, 200);  

		ctx.beginPath();  
		ctx.strokeStyle = \"rgb(255, 0, 0)\";  
		ctx.shadowColor = \"rgba(255, 128, 128, 0.5)\";  
		ctx.arc(100, 100, 90, startAngle, (hours / 6 + minutes / 360 + seconds / 21600 - 0.5) * Math.PI, false);  
		ctx.stroke();  

		ctx.beginPath();  
		ctx.strokeStyle = \"rgb(0, 255, 0)\";  
		ctx.shadowColor = \"rgba(128, 255, 128, 0.5)\";  
		ctx.arc(100, 100, 75,startAngle, (minutes / 30 + seconds / 1800 - 0.5) * Math.PI, false);  
		ctx.stroke();  

		ctx.beginPath();  
		ctx.strokeStyle = \"rgb(0, 0, 255)\";  
		ctx.shadowColor = \"rgba(128, 128, 255, 0.5)\";  
		ctx.arc(100, 100, 60,startAngle, (seconds / 30 - 0.5) * Math.PI, false);  
		ctx.stroke();  

		time = [];  
		if (hours < 10) {  
			time.push('0');  
		}  
		time.push(hours);  
		time.push(':');  

		if (minutes < 10) {  
			time.push('0');  
		}  
		time.push(minutes);  
		time.push(':');  

		if (seconds < 10) {  
			time.push('0');  
		}  
		time.push(seconds);  

		if (am) {  
			time.push('AM');  
		} else {  
			time.push('PM');  
		}  

		ctx.fillText(time.join(''), 50, 105);  
	}  

	drawClock();  
	setInterval(drawClock, 1000);  
}  
</script>  
</body><br /><br />  
<button type=\"button\"><a href=\"/xinwen/\">新闻</a></button>
<button type=\"button\"><a href=\"/youxi/\">游戏</a></button>
<button type=\"button\"><a href=\"/yinyue/\">音乐</a></button>
<button type=\"button\"><a href=\"/wenxue/\">文学</a></button>
<button type=\"button\"><a href=\"/tupian/\">图片</a></button>
<button type=\"button\"><a href=\"/youxiang/\">邮箱</a></button>
<button type=\"button\"><a href=\"/gouwu/\">购物</a></button>
<button type=\"button\"><a href=\"/jiaoliu/\">交流</a></button>
<br /><br /><br />
<form action=\"http://www.baidu.com/baidu\" target=\"_blank\">
<table bgcolor=\"#FFFFFF\"><tr><td>
<input name=tn type=hidden value=baidu>
<a href=\"www.baidu.com/\"><img src=\"../static/baidu_jgylogo3.gif\" alt=\"Baidu\" align=\"bottom\" border=\"0\"></a>
<input type=text name=word size=30>
<input type=\"submit\" value=\"百度搜索\">
</td></tr></table>
</form>
</center>
</html>" o)
(define method (read i))
(define url (read i))
(print method)
(print url)
(if (eqv? 'GET method)
  (print method))
(if (eqv? '/xinwen/ url)
  (begin
    (print "ok")
    (write-line "HTTP/1.1 200 OK" o)
    (write-line "" o)
  (write-line "<h3>ni hao</h3>" o)))
(print (read-line i))
(close-input-port i)
(close-output-port o)
