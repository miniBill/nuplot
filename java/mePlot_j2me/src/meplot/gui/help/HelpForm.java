package meplot.gui.help;

import java.io.IOException;
import java.io.Reader;

import javax.microedition.lcdui.Command;
import javax.microedition.lcdui.Display;
import javax.microedition.lcdui.Font;
import javax.microedition.lcdui.Form;
import javax.microedition.lcdui.Item;
import javax.microedition.lcdui.ItemCommandListener;
import javax.microedition.lcdui.StringItem;
import javax.microedition.lcdui.TextField;
import javax.xml.parsers.FactoryConfigurationError;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import meplot.gui.CommandHandler;
import meplot.gui.MidletMenju;
import meplot.help.GuidePages;
import meplot.parser.utils.Cleaner;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

import platform.log.Log;
import platform.log.LogLevel;

public final class HelpForm extends Form implements CommandHandler{
	public class GuideHandler extends DefaultHandler{
		private final class LinkListener implements ItemCommandListener{
			private final String target;

			private LinkListener(final String target){
				this.target = target;
			}

			public void commandAction(final Command arg0, final Item arg1){
				if(target.startsWith("#")){
					final String targetName = target.substring(1);
					final Item targetItem = names.get(targetName);
					if(targetItem != null){
						final Display display = MidletMenju.getDisplay();
						display.setCurrentItem(targetItem);
						display.setCurrent(HelpForm.this);
					}
				}
				else
					loadPage(target);
			}
		}

		public void startElement(final String uri, final String localName,
				final String qName, final Attributes attributes)
				throws SAXException{
			if(buf.length() > 0){
				final String sbuf = getAndCleanBuffer();
				if(!sbuf.equals(" ")){
					final StringItem stringItem = new StringItem("",
							clean(sbuf));
					append(stringItem);
				}
			}

			buf = new StringBuffer();

			if(qName.equals("a")){
				atarget = attributes.getValue("href");
				aname = attributes.getValue("name");
			}
		}

		private String clean(final String string){
			String next = string;
			next = Cleaner.replace(string, "\n", " ");
			next = Cleaner.cut(next, "  ", " ");
			return next;
		}

		public void characters(final char[] ch, final int start,
				final int length) throws SAXException{
			buf.append(ch, start, length);
		}

		private StringBuffer buf = new StringBuffer();

		private String aname = "";
		private String atarget = "";

		private int h2 = 0;
		private int h3 = 0;

		private final StringItemDictionary names = new StringItemDictionary();

		public void endElement(final String uri, final String localName,
				final String qName) throws SAXException{
			if(qName.equals("style")){
				buf = new StringBuffer();
				return;
			}
			StringItem stringItem = null;
			if(qName.equals("h1")){
				h2 = h3 = 0;
				stringItem = new StringItem(getAndCleanBuffer(), "");
			}
			if(qName.equals("h2")){
				h2++;
				h3 = 0;
				final String sbuf = getAndCleanBuffer();
				stringItem = new StringItem(Integer.toString(h2), sbuf);
			}
			if(qName.equals("h3")){
				h3++;
				final StringBuffer label = new StringBuffer();
				label.append(h2);
				label.append('.');
				label.append(h3);
				stringItem = new StringItem(label.toString(),
						getAndCleanBuffer());
			}
			if(qName.equals("li"))
				stringItem = new StringItem("* ", getAndCleanBuffer());
			if(qName.equals("pre"))
				stringItem = new StringItem("", getAndCleanBuffer());
			if(atarget != null && atarget.length() > 0 && stringItem != null){
				final StringItem oldItem = stringItem;
				stringItem = new StringItem(oldItem.getLabel(),
						oldItem.getText(), Item.HYPERLINK);
				final Command cmd = new Command("Go", Command.ITEM, 1);
				stringItem.addCommand(cmd);
				stringItem.setDefaultCommand(cmd);
				stringItem.setItemCommandListener(new LinkListener(atarget));
				atarget = "";
			}
			if(aname != null && aname.length() > 0 && stringItem != null){
				names.add(aname, stringItem);
				aname = "";
			}
			if(stringItem != null){
				if(qName.equals("h1") || qName.equals("h2")
						|| qName.equals("h3"))
					stringItem.setLayout(Item.LAYOUT_CENTER
							| Item.LAYOUT_NEWLINE_AFTER
							| Item.LAYOUT_NEWLINE_BEFORE);
				else
					if(qName.equals("pre")){
						stringItem.setLayout(Item.LAYOUT_NEWLINE_AFTER
								| Item.LAYOUT_NEWLINE_BEFORE);
						stringItem.setFont(Font.getFont(Font.FACE_MONOSPACE,
								Font.STYLE_PLAIN, Font.SIZE_MEDIUM));
					}
				stringItem.setLayout(Item.LAYOUT_LEFT);
				HelpForm.this.append(stringItem);
			}
		}

		private String getAndCleanBuffer(){
			final String sbuf = clean(buf.toString());
			buf = new StringBuffer();
			return sbuf;
		}

		public void error(final SAXParseException e) throws SAXException{
			Log.log(e);
			setTitle(getTitle() + ", error: " + e.getMessage());
		}

		public void endDocument() throws SAXException{
			Log.log(LogLevel.WARNING, "endDocument");
		}

		public void fatalError(final SAXParseException e) throws SAXException{
			Log.log(e);
			setTitle(getTitle() + ", fatalError: " + e.getMessage());
		}

		public void warning(final SAXParseException e) throws SAXException{
			Log.log(e);
			setTitle(getTitle() + ", warning: " + e.getMessage());
		}
	}

	public HelpForm(){
		super("Help");

		reset();
	}

	public void reset(){
		backlog.clear();
		loadPage("guide");
	}

	private void loadPage(final String page){
		backlog.push(page);

		deleteAll();

		final String firstPage = GuidePages.getPage(page);

		final StringBuffer error = new StringBuffer();

		SAXParser parser = null;
		try{
			parser = SAXParserFactory.newInstance().newSAXParser();
		}
		catch(final ParserConfigurationException e){
			Log.log(e);
			error.append(e.getMessage());
		}
		catch(final SAXException e){
			Log.log(e);
			error.append(e.getMessage());
		}
		catch(final FactoryConfigurationError e){
			Log.log(e);
			error.append(e.getMessage());
		}

		if(error.length() == 0 && parser != null){
			final Reader characterStream = new StringReader(firstPage);
			final InputSource source = new InputSource(characterStream);
			final DefaultHandler handler = new GuideHandler();
			try{
				parser.parse(source, handler);
			}
			catch(final SAXException e){
				Log.log(e);
				error.append(e.getMessage());
			}
			catch(final IOException e){
				Log.log(e);
				error.append(e.getMessage());
			}
		}

		if(error.length() > 0){
			final TextField errorField = new TextField("Error",
					error.toString(), 500, TextField.UNEDITABLE);
			this.append(errorField);
		}
	}

	private final StringStack backlog = new StringStack();

	public boolean handle(final Command command){
		if(command.equals(MidletMenju.getBackCommand())){
			backlog.pop();
			if(backlog.isEmpty())
				return false;
			loadPage(backlog.pop());
			return true;
		}
		return false;
	}

	public void addCommands(final MidletMenju midletMenju){
		addCommand(MidletMenju.getBackCommand());

		setCommandListener(midletMenju);
	}
}
