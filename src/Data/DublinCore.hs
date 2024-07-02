{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : DublinCore
Description : Dublin core metadata terms
Copyright   : (c) Doug Palmer
2024
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

URIs for dublin core metadata
-}
module Data.DublinCore (module Data.DublinCore) where


import Data.Localised (textToUri)
import Network.URI (URI)

-- Dublin core elements

dcContributor :: URI
dcContributor = textToUri "http://purl.org/dc/elements/1.1/contributor"
dcCoverage :: URI
dcCoverage = textToUri "http://purl.org/dc/elements/1.1/coverage"
dcCreator :: URI
dcCreator = textToUri "http://purl.org/dc/elements/1.1/creator"
dDdate :: URI
dDdate = textToUri "http://purl.org/dc/elements/1.1/date"
dcDescription :: URI
dcDescription = textToUri "http://purl.org/dc/elements/1.1/description"
dcFormat :: URI
dcFormat = textToUri "http://purl.org/dc/elements/1.1/format"
dcIdentifier :: URI
dcIdentifier = textToUri "http://purl.org/dc/elements/1.1/identifier"
dcLanguage :: URI
dcLanguage = textToUri "http://purl.org/dc/elements/1.1/language"
dcPublisher :: URI
dcPublisher = textToUri "http://purl.org/dc/elements/1.1/publisher"
dcRelation :: URI
dcRelation = textToUri "http://purl.org/dc/elements/1.1/relation"
dcRights :: URI
dcRights = textToUri "http://purl.org/dc/elements/1.1/rights"
dcSource :: URI
dcSource = textToUri "http://purl.org/dc/elements/1.1/source"
dcSubject :: URI
dcSubject = textToUri "http://purl.org/dc/elements/1.1/subject"
dcTitle :: URI
dcTitle = textToUri "http://purl.org/dc/elements/1.1/title"
dcType :: URI
dcType = textToUri "http://purl.org/dc/elements/1.1/type"

-- Dublin core terms

dctermsAbstract :: URI
dctermsAbstract = textToUri "http://purl.org/dc/terms/abstract"
dctermsAccessRights :: URI
dctermsAccessRights = textToUri "http://purl.org/dc/terms/accessRights"
dctermsAccrualMethod :: URI
dctermsAccrualMethod = textToUri "http://purl.org/dc/terms/accrualMethod"
dctermsAccrualPeriodicity :: URI
dctermsAccrualPeriodicity = textToUri "http://purl.org/dc/terms/accrualPeriodicity"
dctermsAccrualPolicy :: URI
dctermsAccrualPolicy = textToUri "http://purl.org/dc/terms/accrualPolicy"
dctermsAlternative :: URI
dctermsAlternative = textToUri "http://purl.org/dc/terms/alternative"
dctermsAudience :: URI
dctermsAudience = textToUri "http://purl.org/dc/terms/audience"
dctermsAvailable :: URI
dctermsAvailable = textToUri "http://purl.org/dc/terms/available"
dctermsBibliographicCitation :: URI
dctermsBibliographicCitation = textToUri "http://purl.org/dc/terms/bibliographicCitation"
dctermsConformsTo :: URI
dctermsConformsTo = textToUri "http://purl.org/dc/terms/conformsTo"
dctermsContributor :: URI
dctermsContributor = textToUri "http://purl.org/dc/terms/contributor"
dctermsCoverage :: URI
dctermsCoverage = textToUri "http://purl.org/dc/terms/coverage"
dctermsCreated :: URI
dctermsCreated = textToUri "http://purl.org/dc/terms/created"
dctermsCreator :: URI
dctermsCreator = textToUri "http://purl.org/dc/terms/creator"
dctermsDate :: URI
dctermsDate = textToUri "http://purl.org/dc/terms/date"
dctermsDateAccepted :: URI
dctermsDateAccepted = textToUri "http://purl.org/dc/terms/dateAccepted"
dctermsDateCopyrighted :: URI
dctermsDateCopyrighted = textToUri "http://purl.org/dc/terms/dateCopyrighted"
dctermsDateSubmitted :: URI
dctermsDateSubmitted = textToUri "http://purl.org/dc/terms/dateSubmitted"
dctermsDescription :: URI
dctermsDescription = textToUri "http://purl.org/dc/terms/description"
dctermsEducationLevel :: URI
dctermsEducationLevel = textToUri "http://purl.org/dc/terms/educationLevel"
dctermsExtent :: URI
dctermsExtent = textToUri "http://purl.org/dc/terms/extent"
dctermsFormat :: URI
dctermsFormat = textToUri "http://purl.org/dc/terms/format"
dctermsHasFormat :: URI
dctermsHasFormat = textToUri "http://purl.org/dc/terms/hasFormat"
dctermsHasPart :: URI
dctermsHasPart = textToUri "http://purl.org/dc/terms/hasPart"
dctermsHasVersion :: URI
dctermsHasVersion = textToUri "http://purl.org/dc/terms/hasVersion"
dctermsIdentifier :: URI
dctermsIdentifier = textToUri "http://purl.org/dc/terms/identifier"
dctermsInstructionalMethod :: URI
dctermsInstructionalMethod = textToUri "http://purl.org/dc/terms/instructionalMethod"
dctermsIsFormatOf :: URI
dctermsIsFormatOf = textToUri "http://purl.org/dc/terms/isFormatOf"
dctermsIsPartOf :: URI
dctermsIsPartOf = textToUri "http://purl.org/dc/terms/isPartOf"
dctermsIsReferencedBy :: URI
dctermsIsReferencedBy = textToUri "http://purl.org/dc/terms/isReferencedBy"
dctermsIsReplacedBy :: URI
dctermsIsReplacedBy = textToUri "http://purl.org/dc/terms/isReplacedBy"
dctermsIsRequiredBy :: URI
dctermsIsRequiredBy = textToUri "http://purl.org/dc/terms/isRequiredBy"
dctermsIssued :: URI
dctermsIssued = textToUri "http://purl.org/dc/terms/issued"
dctermsIsVersionOf :: URI
dctermsIsVersionOf = textToUri "http://purl.org/dc/terms/isVersionOf"
dctermsLanguage :: URI
dctermsLanguage = textToUri "http://purl.org/dc/terms/language"
dctermsLicense :: URI
dctermsLicense = textToUri "http://purl.org/dc/terms/license"
dctermsMediator :: URI
dctermsMediator = textToUri "http://purl.org/dc/terms/mediator"
dctermsMedium :: URI
dctermsMedium = textToUri "http://purl.org/dc/terms/medium"
dctermsModified :: URI
dctermsModified = textToUri "http://purl.org/dc/terms/modified"
dctermsProvenance :: URI
dctermsProvenance = textToUri "http://purl.org/dc/terms/provenance"
dctermsPublisher :: URI
dctermsPublisher = textToUri "http://purl.org/dc/terms/publisher"
dctermsReferences :: URI
dctermsReferences = textToUri "http://purl.org/dc/terms/references"
dctermsRelation :: URI
dctermsRelation = textToUri "http://purl.org/dc/terms/relation"
dctermsReplaces :: URI
dctermsReplaces = textToUri "http://purl.org/dc/terms/replaces"
dctermsRequires :: URI
dctermsRequires = textToUri "http://purl.org/dc/terms/requires"
dctermsRights :: URI
dctermsRights = textToUri "http://purl.org/dc/terms/rights"
dctermsRightsHolder :: URI
dctermsRightsHolder = textToUri "http://purl.org/dc/terms/rightsHolder"
dctermsSource :: URI
dctermsSource = textToUri "http://purl.org/dc/terms/source"
dctermsSpatial :: URI
dctermsSpatial = textToUri "http://purl.org/dc/terms/spatial"
dctermsSubject :: URI
dctermsSubject = textToUri "http://purl.org/dc/terms/subject"
dctermsTableOfContents :: URI
dctermsTableOfContents = textToUri "http://purl.org/dc/terms/tableOfContents"
dctermsTemporal :: URI
dctermsTemporal = textToUri "http://purl.org/dc/terms/temporal"
dctermsTitle :: URI
dctermsTitle = textToUri "http://purl.org/dc/terms/title"
dctermsType :: URI
dctermsType = textToUri "http://purl.org/dc/terms/type"
dctermsValid :: URI
dctermsValid = textToUri "http://purl.org/dc/terms/valid"
