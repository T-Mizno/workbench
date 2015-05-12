import java.io.*;
import java.util.*;

public class Test0
{
  public static void main(String[] argv)
    {
      if(argv.length<1) return;

      String title="test img";
      String backIndex="./index.html";
      MFilter filter = new MFilter(".jpg");


      File targetDir = new File(argv[0]);
      String[] files = targetDir.list(filter);
      Arrays.sort(files);
      
      int htmlPageCount=0;
      int imgCount = 0;

      for(int i=0; i<files.length; i++)
	{
	  System.out.println(files[i]);
	}
      while(true)
	{
	  try
	    {
	      if(imgCount >= files.length) break;

	      FileOutputStream stream = new FileOutputStream(htmlPageCount+".html");
	      OutputStreamWriter writer = new OutputStreamWriter(stream, "euc-jp");
	      PrintWriter print = new PrintWriter(writer);
	      print.println("<HTML>");
	      print.println("<HEAD>");
	      print.println("<TITLE>"+title+"　("+(imgCount+1)+"/"+files.length+")</TITLE>");
	      print.println("<META http-equiv=\"Content-Type\" content=\"text/html; charset=euc-jp\">");
	      print.println("</HEAD>");
	      print.println("</HTML>");
	      print.println("<BODY>");
	      
	      print.println("<CENTER>");
	      print.print("<H1>"+title);
	      print.print("　("+(imgCount+1)+"/"+files.length+")");
	      print.println("</H1>");

	      while(true)
		{
		  if(imgCount >= files.length) break;
		  print.println("<IMG src=\""+files[imgCount]+"\" alt="
				+"\""+files[imgCount]+"\" >");
		  imgCount++;
		  break;
		}

	      print.println("<BR>");

	      if(htmlPageCount > 0)
		{
		  print.println("<A href=\"./"+(htmlPageCount-1)+".html\">前へ</A>");
		}
	      if(imgCount < files.length)
		{
		  print.println("<A href=\"./"+(htmlPageCount+1)+".html\">次へ</A>");
		}

	      print.println("<HR>");
	      print.println("<A href=\""+backIndex+"\">メニューへ戻る</A>");
	      print.println("</BODY>");
	      print.println("</HTML>");
	      print.println("</CENTER>");
	      print.close();
	      writer.close();
	      stream.close();
	    }
	  catch(Exception e)
	    {
	      e.printStackTrace();
	    }
	  htmlPageCount++;
	}
    }
}

class MFilter implements FilenameFilter
{
  String postfix = ".jpg";

  MFilter(String _postfix)
    {
      postfix = _postfix;
    }

  public boolean accept(File f, String name)
    {
      if(name.endsWith(postfix)) return true;
      return false;
    }
}
