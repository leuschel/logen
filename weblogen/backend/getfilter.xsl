<?xml version="1.0" ?>
<!DOCTYPE xsl:stylesheet 
[ 
<!ENTITY nbsp "&#160;"> <!-- white space in XSL -->
]>   
<xsl:stylesheet version = '1.0'
	xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

	<xsl:output method="text" media-type="text/plain"
				indent="no" encoding="UTF8"/>

	<xsl:template match="/">
		<xsl:apply-templates/>
	</xsl:template>

	<xsl:template match="source">
	</xsl:template>

	<xsl:template match="filters">
		<xsl:apply-templates/>
	</xsl:template>
	
</xsl:stylesheet> 
