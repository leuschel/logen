<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>   
<xsl:stylesheet version='1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

	<xsl:output method="html" media-type="text/html"
				indent="no" encoding="UTF8"/>

	<xsl:template match="/">
		<xsl:apply-templates/>
	</xsl:template>

	<xsl:template match="error">
		<xsl:apply-templates/>
	</xsl:template>

	<xsl:template match="title">
		<h2>
			<xsl:apply-templates/>
		</h2>
	</xsl:template>

	<xsl:template match="details">
		<p>&nbsp;
			<xsl:apply-templates/>
		</p>
	</xsl:template>

	<xsl:template match="code">
		<xsl:text disable-output-escaping="yes">&amp;lsquo;</xsl:text>
		<span class="code">
			<xsl:apply-templates/>
		</span>
		<xsl:text disable-output-escaping="yes">&amp;rsquo;</xsl:text>
	</xsl:template>
</xsl:stylesheet> 
