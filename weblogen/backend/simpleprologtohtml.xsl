<?xml version="1.0" ?>
<!DOCTYPE xsl:stylesheet 
[ 
<!ENTITY nbsp "&#160;"> <!-- white space in XSL -->
]>   
<xsl:stylesheet version = '1.0'
	xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

	<xsl:output method="html" media-type="text/html"
				indent="no" encoding="UTF8"/>

	<xsl:template match="/">
		<xsl:apply-templates/>
	</xsl:template>

	<xsl:template match="source">
		<pre id="source">
		<xsl:apply-templates/>
		</pre>
	</xsl:template>

	<xsl:template match="nbsp">
		<xsl:text disable-output-escaping="yes">&amp;nbsp;</xsl:text>
	</xsl:template>

	<!-- Hack to convert comm to comment as pillow converts comment() to
		 XML comments which is not what we want -->
	<xsl:template match="comm">
		<xsl:element name="span">
			<xsl:attribute name="class">
				<xsl:text>comment</xsl:text>
			</xsl:attribute>
			<xsl:value-of select="."/>
		</xsl:element>
	</xsl:template> 

	<xsl:template match="head|list|string">
		<xsl:element name="span">
			<xsl:attribute name="class">
				<xsl:value-of select="name()"/>
			</xsl:attribute>
			<xsl:value-of select="."/>
		</xsl:element>
	</xsl:template> 

</xsl:stylesheet> 
