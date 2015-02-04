<?xml version="1.0" ?>
<!DOCTYPE xsl:stylesheet 
[ 
<!ENTITY nbsp "&#160;"> <!-- white space in XSL -->
]>   
<xsl:stylesheet version = '1.0'
	xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

	<xsl:output method="text" media-type="text/html"
				indent="no" encoding="UTF8"/>

	<xsl:template match="/">
		<xsl:apply-templates/>
	</xsl:template>

    <xsl:template match="filters//*[@fix]">
        <xsl:variable name="annotation" select="concat('filt', position())"/>

        <xsl:text>document.getElementById("filt</xsl:text>
        <xsl:number count="*" level="any" from="/"/>
        <xsl:text>").className = "</xsl:text>
        <xsl:value-of select="@fix"/>
        <xsl:text>"; document.getElementById("filt</xsl:text>
        <xsl:number count="*" level="any" from="/"/>
        <xsl:text>").innerHTML = "</xsl:text>
        <xsl:value-of select="@fix"/>
        <xsl:text>";</xsl:text>
    </xsl:template>

    <xsl:template match="source//*[@fix]">
        <xsl:text>document.getElementById("ann</xsl:text>
        <xsl:number count="*" level="any" from="/"/>
        <xsl:text>").className = "</xsl:text>
        <xsl:value-of select="@fix"/>
        <xsl:text>";</xsl:text>
    </xsl:template>

    <xsl:template match="source//fix_hide_nf">
        <xsl:text>document.getElementById("ann</xsl:text>
        <xsl:number count="*" level="any" from="/"/>
        <xsl:text>").className = "hide_nf";</xsl:text>
    </xsl:template>

	<xsl:template match="wholefilter[badfilter/replacement]">
        <xsl:text>document.getElementById("wholefilt</xsl:text>
        <xsl:number count="wholefilter" level="any" from="/"/>
        <xsl:text>").innerHTML = document.getElementById("replace</xsl:text>
        <xsl:number count="wholefilter" level="any" from="/"/>
        <xsl:text>").innerHTML;</xsl:text>
	</xsl:template>

    <xsl:template match="text()">
    </xsl:template>
</xsl:stylesheet> 
